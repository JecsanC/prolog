:- use_module(library(http/html_head)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_files)).

:- dynamic conocido/2.

% Declarar alias para archivos estáticos
:- multifile user:file_search_path/2.
:- dynamic user:file_search_path/2.
user:file_search_path(img, 'img').
user:file_search_path(css, './css').

% Rutas de archivos estáticos
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).

% Handlers principales
:- http_handler(root(.), inicio_handler, []).
:- http_handler(root(jugar), jugar_handler, []).
:- http_handler(root(responder), responder_handler, [method(post)]).

% Iniciar servidor
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Página de inicio
inicio_handler(_Request) :-
    http_session_retractall(conocido(_, _)),
    http_session_retractall(conocido_list(_)),
    http_session_retractall(pregunta_actual(_)),
    reply_html_page(
        [title('Adivina Quién')],
        [ \html_requires(css('styles.css')),
          h1('Bienvenido al juego "Adivina Quién"'),
          p('Haz clic en comenzar para iniciar.'),
          a([href='/jugar'], 'Comenzar')
        ]).

jugar_handler(_Request) :-
    findall(P, personaje(P, _), Todos),
    (   http_session_data(conocido_list(YaPreguntado))
    ->  true
    ;   YaPreguntado = [], http_session_assert(conocido_list([]))
    ),
    jugar_web(YaPreguntado, Todos, Respuesta),
    Respuesta.

jugar_web(YaPreguntado, Personajes, RespuestaHTML) :-
    filtrar_personajes(Personajes, YaPreguntado, Candidatos),
    (   Candidatos = [Unico]
    ->  atomic_list_concat(['img/', Unico, '.jpg'], ImagenPath),
        RespuestaHTML = reply_html_page(title('Resultado'),
            [ \html_requires(css('styles.css')),
              div([
                  h2(['¡Tu personaje es: ', Unico, '!']),
                  img([src=ImagenPath, alt=Unico, class=resultado]),
                  br([]),
                  a([href='/'], 'Jugar de nuevo')
              ])
            ])
    ;   Candidatos = []
    ->  RespuestaHTML = reply_html_page(title('Falló'),
            [ \html_requires(css('styles.css')),
              h2('No pude adivinar el personaje.'),
              a([href='/'], 'Intentar de nuevo')
            ])
    ;   siguiente_pregunta(Candidatos, YaPreguntado, Pregunta),
        http_session_retractall(pregunta_actual(_)),
        http_session_assert(pregunta_actual(Pregunta)),
        RespuestaHTML = reply_html_page(title('Pregunta'),
            [ \html_requires(css('styles.css')),
              h2(['¿El personaje tiene: ', Pregunta, '?']),
              form([action='/responder', method='POST'], [
                  input([type=submit, name=respuesta, value='s']),
                  input([type=submit, name=respuesta, value='n'])
              ])
            ])
    ).

responder_handler(Request) :-
    catch(
        (
            http_parameters(Request, [respuesta(RespStr, [])]),
            (   http_session_data(pregunta_actual(Pregunta))
            ->  true
            ;   reply_html_page(title('Error'),
                    [ h2('Error: no hay pregunta activa.'),
                      a([href='/'], 'Volver al inicio') ]),
                fail
            ),
            (   http_session_data(conocido_list(Previos)) -> true ; Previos = []),
            normalizar_respuesta(RespStr, Pregunta, Resp),
            append(Previos, [conocido(Pregunta, Resp)], Nuevos),
            http_session_retractall(conocido_list(_)),
            http_session_assert(conocido_list(Nuevos)),
            jugar_handler(Request)
        ),
        Error,
        (
            print_message(error, Error),
            reply_html_page(title('Error'),
                [ h2('Se produjo un error procesando la respuesta.'),
                  a([href='/'], 'Volver al inicio') ])
        )
    ).



% ------------------------
% Lógica de inferencia
% ------------------------

filtrar_personajes(Personajes, Conocidos, Filtrados) :-
    include(satisface_todo(Conocidos), Personajes, Filtrados).

satisface_todo([], _).
satisface_todo([conocido(A, V)|Resto], P) :-
    personaje(P, Atributos),
    (   V = no(A) -> \+ member(A, Atributos)
    ;   member(V, Atributos)
    ),
    satisface_todo(Resto, P).

siguiente_pregunta(Candidatos, YaPreguntado, Mejor) :-
    findall(Attr, (member(P, Candidatos), personaje(P, Lista), member(Attr, Lista)), Aplanado),
    sort(Aplanado, Unicos),
    exclude(ya_preguntado(YaPreguntado), Unicos, CandidatosNuevos),
    mejor_atributo(CandidatosNuevos, Candidatos, Mejor).

ya_preguntado(Conocidos, A) :-
    member(conocido(_, A), Conocidos);
    member(conocido(A, _), Conocidos);
    member(conocido(_, no(A)), Conocidos).

mejor_atributo([], _, desconocido).
mejor_atributo([A|R], Candidatos, Mejor) :-
    contar_ocurrencias(A, Candidatos, CountA),
    mejor_atributo(R, Candidatos, Temp),
    length(Candidatos, Total),
    CountComplementA is Total - CountA,
    (   Temp = desconocido -> Mejor = A
    ;   contar_ocurrencias(Temp, Candidatos, CountT),
        CountComplementT is Total - CountT,
        abs(CountA - CountComplementA) < abs(CountT - CountComplementT)
    ->  Mejor = A
    ;   Mejor = Temp
    ).

contar_ocurrencias(A, Candidatos, N) :-
    include(has_attr(A), Candidatos, Coinciden),
    length(Coinciden, N).

has_attr(A, P) :- personaje(P, L), member(A, L).

normalizar_respuesta("s", A, A).
normalizar_respuesta("n", A, no(A)).

% ------------------------
% Base de datos de personajes
% ------------------------

% personaje(juan,     [hombre, pelo_negro, ojos_cafe, usa_lentes, alto, barba, sombrero]).
personaje(maria,    [mujer, pelo_rubio, ojos_azules, usa_lentes, baja, vestido, joyas]).
% personaje(carlos,   [hombre, pelo_rojo, ojos_verdes, no_lentes, mediano, bigote, camisa]).
% personaje(ana,      [mujer, pelo_castano, ojos_cafe, no_lentes, alta, falda, mochila]).
% personaje(pedro,    [hombre, pelo_negro, ojos_negros, usa_lentes, bajo, gorra, sudadera]).
% personaje(laura,    [mujer, pelo_morado, ojos_verdes, no_lentes, mediana, piercing, chaqueta]).
% personaje(ricardo,  [hombre, pelo_cano, ojos_azules, usa_lentes, alto, corbata, reloj]).
% personaje(sofia,    [mujer, pelo_rosa, ojos_cafe, no_lentes, baja, aretes, vestido_largo]).
% personaje(andres,   [hombre, pelo_verde, ojos_grises, usa_lentes, mediano, tatuaje, bufanda]).
% personaje(beatriz,  [mujer, pelo_azul, ojos_avellana, no_lentes, alta, collar, blusa]).
% personaje(fernando, [hombre, pelo_negro, ojos_cafe, no_lentes, alto, cicatriz, uniforme]).
% personaje(diana,    [mujer, pelo_rubio, ojos_azules, usa_lentes, mediana, diadema, abrigo]).
% personaje(jorge,    [hombre, pelo_blanco, ojos_grises, usa_lentes, bajo, baston, sombrero]).
% personaje(elena,    [mujer, pelo_negro, ojos_verdes, no_lentes, mediana, bufanda, blusa]).
% personaje(sergio,   [hombre, pelo_castano, ojos_azules, no_lentes, alto, gorra, chaqueta]).
% personaje(patricia, [mujer, pelo_rojo, ojos_cafe, usa_lentes, baja, vestido, aretes]).
% personaje(luis,     [hombre, pelo_rubio, ojos_avellana, usa_lentes, mediano, bigote, camisa]).
% personaje(claudia,  [mujer, pelo_castano, ojos_grises, no_lentes, alta, falda, collar]).
% personaje(eduardo,  [hombre, pelo_negro, ojos_verdes, no_lentes, bajo, sudadera, tatuaje]).
% personaje(raquel,   [mujer, pelo_azul, ojos_azules, usa_lentes, mediana, diadema, abrigo]).
% personaje(hector,   [hombre, pelo_morado, ojos_negros, usa_lentes, alto, corbata, reloj]).
% personaje(isabel,   [mujer, pelo_rosa, ojos_cafe, no_lentes, baja, mochila, piercing]).
% personaje(alejandro,[hombre, pelo_verde, ojos_avellana, no_lentes, mediano, uniforme, cicatriz]).
% personaje(natalia,  [mujer, pelo_blanco, ojos_verdes, usa_lentes, alta, blusa, joyas]).
% personaje(omar,     [hombre, pelo_rojo, ojos_grises, no_lentes, bajo, sudadera, gorra]).
