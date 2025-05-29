:- dynamic conocido/2.

% Base de conocimientos con caracteristicas fisicas
personaje(juan,     [hombre, pelo_negro, ojos_cafe, usa_lentes, alto, barba, sombrero]).
personaje(maria,    [mujer, pelo_rubio, ojos_azules, usa_lentes, baja, vestido, joyas]).
personaje(carlos,   [hombre, pelo_rojo, ojos_verdes, no_lentes, mediano, bigote, camisa]).
personaje(ana,      [mujer, pelo_castano, ojos_cafe, no_lentes, alta, falda, mochila]).
personaje(pedro,    [hombre, pelo_negro, ojos_negros, usa_lentes, bajo, gorra, sudadera]).
personaje(laura,    [mujer, pelo_morado, ojos_verdes, no_lentes, mediana, piercing, chaqueta]).
personaje(ricardo,  [hombre, pelo_cano, ojos_azules, usa_lentes, alto, corbata, reloj]).
personaje(sofia,    [mujer, pelo_rosa, ojos_cafe, no_lentes, baja, aretes, vestido_largo]).
personaje(andres,   [hombre, pelo_verde, ojos_grises, usa_lentes, mediano, tatuaje, bufanda]).
personaje(beatriz,  [mujer, pelo_azul, ojos_avellana, no_lentes, alta, collar, blusa]).
personaje(fernando, [hombre, pelo_negro, ojos_cafe, no_lentes, alto, cicatriz, uniforme]).
personaje(diana,    [mujer, pelo_rubio, ojos_azules, usa_lentes, mediana, diadema, abrigo]).
personaje(jorge,    [hombre, pelo_blanco, ojos_grises, usa_lentes, bajo, baston, sombrero]).
personaje(elena,    [mujer, pelo_negro, ojos_verdes, no_lentes, mediana, bufanda, blusa]).
personaje(sergio,   [hombre, pelo_castano, ojos_azules, no_lentes, alto, gorra, chaqueta]).
personaje(patricia, [mujer, pelo_rojo, ojos_cafe, usa_lentes, baja, vestido, aretes]).
personaje(luis,     [hombre, pelo_rubio, ojos_avellana, usa_lentes, mediano, bigote, camisa]).
personaje(claudia,  [mujer, pelo_castano, ojos_grises, no_lentes, alta, falda, collar]).
personaje(eduardo,  [hombre, pelo_negro, ojos_verdes, no_lentes, bajo, sudadera, tatuaje]).
personaje(raquel,   [mujer, pelo_azul, ojos_azules, usa_lentes, mediana, diadema, abrigo]).
personaje(hector,   [hombre, pelo_morado, ojos_negros, usa_lentes, alto, corbata, reloj]).
personaje(isabel,   [mujer, pelo_rosa, ojos_cafe, no_lentes, baja, mochila, piercing]).
personaje(alejandro,[hombre, pelo_verde, ojos_avellana, no_lentes, mediano, uniforme, cicatriz]).
personaje(natalia,  [mujer, pelo_blanco, ojos_verdes, usa_lentes, alta, blusa, joyas]).
personaje(omar,     [hombre, pelo_rojo, ojos_grises, no_lentes, bajo, sudadera, gorra]).


% Juego principal
iniciar :-
    retractall(conocido(_, _)),
    writeln('Bienvenido al juego "Adivina Quien"!'),
    jugar([], Personaje),
    (   Personaje = ninguno
    ->  writeln('No pude adivinar el personaje. Intenta con otras respuestas.')
    ;   format('Creo que estas pensando en: ~w!~n', [Personaje])
    ).

% Bucle de inferencia
jugar(YaPreguntado, Personaje) :-
    findall(P, personaje(P, _), Todos),
    filtrar_personajes(Todos, YaPreguntado, Candidatos),
    (   Candidatos = [Unico]
    ->  Personaje = Unico
    ;   Candidatos = []
    ->  Personaje = ninguno
    ;   siguiente_pregunta(Candidatos, YaPreguntado, Pregunta),
        hacer_pregunta(Pregunta, Respuesta),
        jugar([conocido(Pregunta, Respuesta)|YaPreguntado], Personaje)
    ).

% Filtra personajes segun las respuestas conocidas
filtrar_personajes(Personajes, Conocidos, Filtrados) :-
    include(satisface_todo(Conocidos), Personajes, Filtrados).

satisface_todo([], _).
satisface_todo([conocido(A, V)|Resto], P) :-
    personaje(P, Atributos),
    (   V = no(A) -> \+ member(A, Atributos)
    ;   member(V, Atributos)
    ),
    satisface_todo(Resto, P).

% Selecciona el atributo que mas discrimina
siguiente_pregunta(Candidatos, YaPreguntado, Mejor) :-
    findall(Attr, (member(P, Candidatos), personaje(P, Lista), member(Attr, Lista)), Aplanado),
    sort(Aplanado, Unicos),
    exclude(ya_preguntado(YaPreguntado), Unicos, CandidatosNuevos),
    mejor_atributo(CandidatosNuevos, Candidatos, Mejor).

ya_preguntado(Conocidos, A) :- member(conocido(_, A), Conocidos); member(conocido(A, _), Conocidos); member(conocido(_, no(A)), Conocidos).

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

% Realiza pregunta y obtiene respuesta
hacer_pregunta(A, Respuesta) :-
    format('El personaje tiene esta caracteristica: ~w? (s/n): ', [A]),
    read_line_to_string(user_input, Raw),
    normalizar_respuesta(Raw, A, Respuesta).

normalizar_respuesta("s", A, A).
normalizar_respuesta("n", A, no(A)).

% Punto de entrada
:- initialization(iniciar, main).
