:- prolog_load_context(directory, Directory),
    atom_concat(Directory, '/src/epictetus', Src),
    asserta(user:file_search_path(epictetus, Src)).
:- use_module(epictetus(main)).
