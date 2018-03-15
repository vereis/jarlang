const erlgar = function () {
    'use_strict';
    const exports = {
        'init': function () {
            const args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
            case 0:
                if (Process.isProcess(this)) {
                    return functions['init/0'].bind(this)(...args);
                } else {
                    return jrts.spawn(function () {
                        return functions['init/0'].bind(this)(...args);
                    });
                }
            }
            throw '** exception error: undefined function' + ('init' + ('/' + arguments.length));
        },
        'module_info': function () {
            const args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
            case 0:
                if (Process.isProcess(this)) {
                    return functions['module_info/0'].bind(this)(...args);
                } else {
                    return jrts.spawn(function () {
                        return functions['module_info/0'].bind(this)(...args);
                    });
                }
            case 1:
                if (Process.isProcess(this)) {
                    return functions['module_info/1'].bind(this)(...args);
                } else {
                    return jrts.spawn(function () {
                        return functions['module_info/1'].bind(this)(...args);
                    });
                }
            }
            throw '** exception error: undefined function' + ('module_info' + ('/' + arguments.length));
        },
        'update': function () {
            const args = [...arguments].map(arg => jrts.jsToErlang(arg));
            switch (arguments.length) {
            case 0:
                if (Process.isProcess(this)) {
                    return functions['update/0'].bind(this)(...args);
                } else {
                    return jrts.spawn(function () {
                        return functions['update/0'].bind(this)(...args);
                    });
                }
            }
            throw '** exception error: undefined function' + ('update' + ('/' + arguments.length));
        }
    };
    const functions = {
        'init/0': function () {
            let _cor0 = erlang['self'].bind(this)();
            erlang['register'].bind(this)(new Atom('game'), _cor0);
            let _tempVar = gameArea.getCanvasSize();
            if (function () {
                    let Width = null, Height = null;
                    if (_tempVar.match(new List(Width, Height).cons(new List()))) {
                        let Width = (_tempVar.nth(0)), Height = (_tempVar.nth(1));
                        return new Atom('true');
                    }
                }.bind(this)()) {
                let Width = null, Height = null;
                Width = (_tempVar.nth(0))
                Height = (_tempVar.nth(1))
                return functions['loop/2'].bind(this)(Width, Height);
            } else if (function () {
                    return new Atom('true');
                }()) {
                let _cor1 = _tempVar;
                throw '** match_fail: TODO Errors dont parse nicely\\n' + 'Message';
            }
        },
        'loop/2': function (_cor1, _cor0) {
            let __mIndex = 0;
            this.setBehaviour(function () {
                let __doNext = true, __doLoop = false;
                do {
                    __doLoop = false;
                    if (__mIndex >= this.messages.length) {
                        this.restartBehaviour();
                        __doNext = false;
                    } else {
                        let __message = this.messages[__mIndex];
                        if (function () {
                                if (__message.match(new Atom('tick'))) {
                                    return new Atom('true');
                                }
                            }.bind(this)()) {
                            this.messages.splice(__mIndex, 1);
                            gameArea['clear'].bind(this)();
                            let _tempVar = gameArea.getMousePos();
                            if (function () {
                                    let PrevX = null, PrevY = null;
                                    if (_tempVar.match(new List(PrevX, PrevY).cons(new List()))) {
                                        let PrevX = (_tempVar.nth(0)), PrevY = (_tempVar.nth(1));
                                        return new Atom('true');
                                    }
                                }.bind(this)()) {
                                console.log(_tempVar);
                                let PrevX = null, PrevY = null;
                                PrevX = (_tempVar.nth(0))
                                PrevY = (_tempVar.nth(1))
                                let _tempVar = get_new_player_pos(_cor1, _cor0, PrevX, PrevY, new Int(50));
                                if (function () {
                                        let PlayerX = null, PlayerY = null;
                                        if (_tempVar.match(new List(PlayerX, PlayerY).cons(new List()))) {
                                            let PlayerX = (_tempVar.nth(0)), PlayerY = (_tempVar.nth(1));
                                            return new Atom('true');
                                        }
                                    }.bind(this)()) {
                                    console.log(_tempVar);
                                    let PlayerX = null, PlayerY = null;
                                    PlayerX = (_tempVar.nth(0))
                                    PlayerY = (_tempVar.nth(1))
                                    let _cor5 = erlang['-'].bind(this)(PlayerX, new Int(5));
                                    let _cor4 = erlang['-'].bind(this)(PlayerY, new Int(5));
                                    gameArea['draw'].bind(this)(new Int(10), new Int(10), new List(new Int(82), new Int(101), new Int(100)), _cor5, _cor4, new List(new Int(115), new Int(113), new Int(117), new Int(97), new Int(114), new Int(101)));
                                    return functions['loop/2'].bind(this)(_cor1, _cor0);
                                } else if (function () {
                                        return new Atom('true');
                                    }()) {
                                    let _cor3 = _tempVar;
                                    throw '** match_fail: TODO Errors dont parse nicely\\n' + 'Message';
                                }
                            } else if (function () {
                                    return new Atom('true');
                                }()) {
                                let _cor2 = _tempVar;
                                throw '** match_fail: TODO Errors dont parse nicely\\n' + 'Message';
                            }
                        } else {
                            __mIndex++;
                            __doLoop = true;
                        }
                    }
                } while (__doLoop);
            });
        },
        'update/0': function () {
            return erlang['!'].bind(this)(new Atom('game'), new Atom('tick'));
        },
        'get_new_player_pos/5': function (_cor4, _cor3, _cor2, _cor1, _cor0) {
            let _cor5 = erlang['/'].bind(this)(_cor4, new Int(2));
            let Dx = erlang['-'].bind(this)(_cor2, _cor5);
            let _cor7 = erlang['/'].bind(this)(_cor3, new Int(2));
            let Dy = erlang['-'].bind(this)(_cor1, _cor7);
            let M = functions['vec_mag/2'].bind(this)(Dx, Dy);
            let _cor12 = erlang['/'].bind(this)(_cor4, new Int(2));
            let _cor10 = erlang['/'].bind(this)(Dx, M);
            let _cor11 = erlang['*'].bind(this)(_cor10, _cor0);
            let _cor13 = erlang['+'].bind(this)(_cor12, _cor11);
            let _cor16 = erlang['/'].bind(this)(_cor3, new Int(2));
            let _cor14 = erlang['/'].bind(this)(Dy, M);
            let _cor15 = erlang['*'].bind(this)(_cor14, _cor0);
            let _cor17 = erlang['+'].bind(this)(_cor16, _cor15);
            return new List(_cor13, _cor17).cons(new List());
        },
        'vec_mag/2': function (_cor1, _cor0) {
            let _cor3 = erlang['*'].bind(this)(_cor1, _cor1);
            let _cor2 = erlang['*'].bind(this)(_cor0, _cor0);
            let _cor4 = erlang['+'].bind(this)(_cor3, _cor2);
            return functions['sqrt/1'].bind(this)(_cor4);
        },
        'sqrt/1': function (_cor0) {
            let _cor1 = erlang['/'].bind(this)(_cor0, new Int(2));
            return functions['sqrt/2'].bind(this)(_cor0, _cor1);
        },
        'sqrt/2': function (_cor1, _cor0) {
            let D = erlang['/'].bind(this)(_cor1, _cor0);
            let _cor3 = erlang['+'].bind(this)(_cor0, D);
            let NG = erlang['/'].bind(this)(_cor3, new Int(2));
            if (function () {
                    return erlang['=:='].bind(this)(NG, _cor0);
                }()) {
                let _cor8 = NG;
                return _cor0;
            } else if (function () {
                    return new Atom('true');
                }()) {
                let _cor9 = NG;
                return functions['sqrt/2'].bind(this)(_cor1, NG);
            }
        },
        'module_info/0': function () {
        },
        'module_info/1': function () {
        }
    };
    return exports;
}();
