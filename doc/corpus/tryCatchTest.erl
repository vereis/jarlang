-module(tryCatchTest).
-compile(export_all).


func(A)->
    catch 2+A;

func(A)->
    try 2+A
    catch
        _ -> foo
    end.