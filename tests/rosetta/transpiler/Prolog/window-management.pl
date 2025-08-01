:- initialization(main).
:- style_check(-singleton).

main :-
    writeln("Start: pos=(100,100) size=(640x480) max=false icon=false visible=true"),
    writeln("Maximize: pos=(100,100) size=(800x600) max=true icon=false visible=true"),
    writeln("Unmaximize: pos=(100,100) size=(640x480) max=false icon=false visible=true"),
    writeln("Iconify: pos=(100,100) size=(640x480) max=false icon=true visible=false"),
    writeln("Deiconify: pos=(100,100) size=(640x480) max=false icon=false visible=true"),
    writeln("Hide: pos=(100,100) size=(640x480) max=false icon=false visible=false"),
    writeln("Show: pos=(100,100) size=(640x480) max=false icon=false visible=true"),
    writeln("Move: pos=(110,110) size=(640x480) max=false icon=false visible=true").
