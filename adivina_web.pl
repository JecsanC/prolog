:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_files)). % ¡Asegúrate de que esta línea esté presente!

:- dynamic conocido/2.

:- http_handler(root(.), inicio_handler, []).
:- http_handler(root(jugar), jugar_handler, []).
:- http_handler(root(responder), responder_handler, [method(post)]).

% ¡CAMBIO AQUÍ! Handler para servir archivos estáticos desde la carpeta 'img'
% Esto mapea la URL /img/ a la carpeta 'img/' en tu proyecto.
:- http_handler(root(img), http_reply_from_files('img/', []), [prefix]).

% ------------------------
% Inicio manual del servidor
% ------------------------
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Opcional: inicia automáticamente al ejecutar el archivo
% :- initialization(server(8080), main).

% ------------------------
% Handlers principales
% ------------------------

inicio_handler(_Request) :-
    http_session_retractall(conocido(_, _)),
    http_session_retractall(conocido_list(_)),
    http_session_retractall(pregunta_actual(_)),
    reply_html_page(
        [title('Adivina Quien')],
        [
            link([rel='stylesheet', href='https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20,100..700,0..1,-50..200&icon_names=rocket']),
            style([
                'body { background-color: #1a1a2e; color: #f0f0f0; font-family: "Press Start 2P", monospace; display: flex; flex-direction: column; align-items: center; justify-content: center; height: 80vh; margin: 0; }',
                'h1 { font-size: 24px; color: #00ffcc; text-shadow: 2px 2px #000; margin-bottom: 20px; }',
                'p { font-size: 14px; color: #f0f0f0; margin: 20px 0; }',
                'a.button {',
                '    background-color: #ff0066;',
                '    color: white;',
                '    padding: 12px 20px;',
                '    text-decoration: none;',
                '    font-size: 15px;',
                '    border-radius: 8px;',
                '    border: 2px solid white;',
                '    box-shadow: 2px 2px #000;',
                '    display: flex;', 
                '    align-items: center;', 
                '    justify-content: center;',
                '    margin-top: 20px;',
                '}',
                'a.button:hover { background-color: #ff3385; }',
                'a.button .material-symbols-outlined {',
                '    font-size: 18px;',
                '    font-variation-settings: "opsz" 24, "wght" 600, "FILL" 1, "GRAD" 0;',
                '    vertical-align: middle;',
                '    margin-left: 8px;', % Espacio entre texto e icono
                '    line-height: 1;',
                '}'
            ]),
            h1(' Bienvenido al juego ¿Y Este Quien Es?'),
            p('Haz clic en comenzar para iniciar.'),
            div([], a([href='/jugar', class='button'], ['Comenzar', span([class='material-symbols-outlined'], 'rocket')]))
        ]
    ).


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
    Estilos = style([
        'body { background-color: #1a1a2e; color: #f0f0f0; font-family: "Press Start 2P", monospace; display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh; margin: 0; text-align: center; }',
        'h2 { font-size: 20px; color: #00ffcc; text-shadow: 2px 2px #000; margin-bottom: 20px; }',
        % Estilo general para enlaces (botones de volver a jugar, etc.)
        'a { background-color: #ff0066; color: white; padding: 10px 20px; text-decoration: none; font-size: 12px; border-radius: 8px; border: 2px solid white; box-shadow: 2px 2px #000; display: inline-block; margin-top: 20px; }',
        'a:hover { background-color: #ff3385; }',
        % Estilo para los botones de Sí/No
        'input[type=submit] { background-color: #00ccff; color: black; font-size: 12px; padding: 10px 20px; border: none; border-radius: 8px; box-shadow: 2px 2px #000; margin: 10px; cursor: pointer; font-family: "Press Start 2P", monospace; }',
        'input[type=submit]:hover { background-color: #33ddff; }',
        % Estilo para la imagen del personaje (¡NUEVO!)
        'img.personaje-img { max-width: 300px; height: auto; border: 4px solid #00ffcc; border-radius: 10px; box-shadow: 0 0 15px rgba(0, 255, 204, 0.7); margin-bottom: 30px; }'
    ]),
    (
        Candidatos = [Unico]
    ->  % ¡Modificación aquí para mostrar la imagen cuando se acierta!
        (   ruta_imagen(Unico, RutaImagen)
        ->  ImagenHTML = img([src=RutaImagen, alt=Unico, class='personaje-img'])
        ;   ImagenHTML = '' % Si no hay imagen, no mostrar nada
        ),
        RespuestaHTML = reply_html_page(title('¡Adiviné!'),
            [ Estilos,
              h2(['¡Tu personaje es: ', Unico, '!']),
              ImagenHTML, % Se inserta la imagen aquí
              a([href='/'], ' Jugar de nuevo')
            ])
    ;   Candidatos = []
    ->  RespuestaHTML = reply_html_page(title('Falló'),
            [ Estilos,
              h2('No pude adivinar el personaje.'),
              a([href='/'], ' Intentar de nuevo')
            ])
    ;   siguiente_pregunta(Candidatos, YaPreguntado, Pregunta),
        http_session_retractall(pregunta_actual(_)),
        http_session_assert(pregunta_actual(Pregunta)),
        RespuestaHTML = reply_html_page(title('Pregunta'),
            [ Estilos,
              h2(['¿El personaje tiene: ', Pregunta, '?']),
              form([action='/responder', method='POST'], [
                  input([type=submit, name=respuesta, value='s']),
                  input([type=submit, name=respuesta, value='n'])
              ])
            ])
    ).


responder_handler(Request) :-
    % format(user_error, 'DEBUG: Entrando a responder_handler~n', []), % Descomenta para depurar
    catch(
        (
            http_parameters(Request, [respuesta(RespStr, [])]),
            % format(user_error, 'DEBUG: Parametro recibido: ~w~n', [RespStr]), % Descomenta para depurar
            (   http_session_data(pregunta_actual(Pregunta))
            ->  % format(user_error, 'DEBUG: Pregunta actual: ~w~n', [Pregunta]) % Descomenta para depurar
                true
            ;   format(user_error, 'ERROR: No hay pregunta_actual~n', []),
                throw(error(no_pregunta_actual))
            ),
            (   http_session_data(conocido_list(Previos))
            ->  true
            ;   Previos = []
            ),
            normalizar_respuesta(RespStr, Pregunta, Resp),
            % format(user_error, 'DEBUG: Respuesta normalizada: ~w~n', [Resp]), % Descomenta para depurar
            append(Previos, [conocido(Pregunta, Resp)], Nuevos),
            http_session_retractall(conocido_list(_)),
            http_session_assert(conocido_list(Nuevos)),
            % format(user_error, 'DEBUG: Nuevos conocidos: ~w~n', [Nuevos]), % Descomenta para depurar
            jugar_handler(Request)
        ),
        Error,
        (
            format(user_error, 'ERROR capturado en responder_handler: ~w~n', [Error]),
            reply_html_page(title('Error'),
                [ h2('Se produjo un error procesando la respuesta.'),
                  pre(['Detalles técnicos: ', Error]),
                  a([href='/'], 'Volver al inicio') ])
        )
    ).


% ------------------------
% Lógica del juego (sin cambios aquí, excepto las adiciones de los predicados de imagen)
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
normalizar_respuesta(s, A, A).
normalizar_respuesta(n, A, no(A)).


% ------------------------
% Base de datos de personajes (25 personajes)
% ------------------------

% ¡NUEVO PREDICADO! Mapea personaje a ruta de imagen
% Las rutas ahora apuntan a la carpeta 'img/'
ruta_imagen(juan, '/img/juan.png').
ruta_imagen(maria, '/img/maria.png').
ruta_imagen(carlos, '/img/carlos.png').
ruta_imagen(ana, '/img/ana.png').
ruta_imagen(pedro, '/img/pedro.png').
ruta_imagen(laura, '/img/laura.jpg').
ruta_imagen(ricardo, '/img/ricardo.jpg').
ruta_imagen(sofia, '/img/sofia.jpg').
ruta_imagen(andres, '/img/andres.jpg').
ruta_imagen(beatriz, '/img/beatriz.jpg').
ruta_imagen(fernando, '/img/fernando.jpg').
ruta_imagen(diana, '/img/diana.jpg').
ruta_imagen(jorge, '/img/jorge.jpg').
ruta_imagen(elena, '/img/elena.jpg').
ruta_imagen(sergio, '/img/sergio.jpg').
ruta_imagen(patricia, '/img/patricia.jpg').
ruta_imagen(luis, '/img/luis.jpg').
ruta_imagen(claudia, '/img/claudia.jpg').
ruta_imagen(eduardo, '/img/eduardo.jpg').
ruta_imagen(raquel, '/img/raquel.jpg').
ruta_imagen(hector, '/img/hector.jpg').
ruta_imagen(isabel, '/img/isabel.jpg').
ruta_imagen(alejandro, '/img/toño.jpg').
ruta_imagen(natalia, '/img/natalia.jpg').
ruta_imagen(omar, '/img/omar.jpg').


% Base de conocimientos con caracteristicas fisicas (sin cambios)
personaje(juan,  [hombre, pelo_negro, ojos_cafe, usa_lentes, alto, barba, sombrero]).
personaje(maria, [mujer, pelo_rubio, ojos_azules, usa_lentes, baja, vestido, joyas]).
personaje(carlos,  [hombre, pelo_rojo, ojos_verdes, no_lentes, mediano, bigote, camisa]).
personaje(ana, [mujer, pelo_castano, ojos_cafe, no_lentes, alta, falda, mochila]).
personaje(pedro, [hombre, pelo_negro, ojos_negros, usa_lentes, bajo, gorra, sudadera]).
personaje(laura, [mujer, pelo_morado, ojos_verdes, no_lentes, mediana, piercing, chaqueta]).
personaje(ricardo, [hombre, pelo_cano, ojos_azules, usa_lentes, alto, corbata, reloj]).
personaje(sofia, [mujer, pelo_rosa, ojos_cafe, no_lentes, baja, aretes, vestido_largo]).
personaje(andres,  [hombre, pelo_verde, ojos_grises, usa_lentes, mediano, tatuaje, bufanda]).
personaje(beatriz, [mujer, pelo_azul, ojos_avellana, no_lentes, alta, collar, blusa]).
personaje(fernando, [hombre, pelo_negro, ojos_cafe, no_lentes, alto, cicatriz, uniforme]).
personaje(diana, [mujer, pelo_rubio, ojos_azules, usa_lentes, mediana, diadema, abrigo]).
personaje(jorge, [hombre, pelo_blanco, ojos_grises, usa_lentes, bajo, baston, sombrero]).
personaje(elena, [mujer, pelo_negro, ojos_verdes, no_lentes, mediana, bufanda, blusa]).
personaje(sergio, [hombre, pelo_castano, ojos_azules, no_lentes, alto, gorra, chaqueta]).
personaje(patricia, [mujer, pelo_rojo, ojos_cafe, usa_lentes, baja, vestido, aretes]).
personaje(luis, [hombre, pelo_rubio, ojos_avellana, usa_lentes, mediano, bigote, camisa]).
personaje(claudia, [mujer, pelo_castano, ojos_grises, no_lentes, alta, falda, collar]).
personaje(eduardo, [hombre, pelo_negro, ojos_verdes, no_lentes, bajo, sudadera, tatuaje]).
personaje(raquel, [mujer, pelo_azul, ojos_azules, usa_lentes, mediana, diadema, abrigo]).
personaje(hector, [hombre, pelo_morado, ojos_negros, usa_lentes, alto, corbata, reloj]).
personaje(isabel, [mujer, pelo_rosa, ojos_cafe, no_lentes, baja, mochila, piercing]).
personaje(alejandro,[hombre, pelo_verde, ojos_avellana, no_lentes, mediano, uniforme, cicatriz]).
personaje(natalia, [mujer, pelo_blanco, ojos_verdes, usa_lentes, alta, blusa, joyas]).
personaje(omar, [hombre, pelo_rojo, ojos_grises, no_lentes, bajo, sudadera, gorra]).