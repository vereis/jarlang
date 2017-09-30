const nnbottles = ( function() {
    const exports = {
        sing: function() {
            switch(arguments.length) {
                case 0:
                    return functions["sing"][0](arguments[0]);
                    break;
                default: 
                    throw ("** exception error: undefined function nnbottles:sing/" + arguments.length);
            }
        }
    };

    const functions = {
        sing: {
            0: function() {
                return functions["sing"][1](99);
            },
            1: function(N) {
                if (N < 0) {
                    return "done.";
                }
                else {
                    functions["verseone"][1](N);
                    functions["versetwo"][1](N - 1);
                    functions["sing"][1](N-1);
                }
            },
        },

        verseone: {
            1: function(N) {
                const Bot = functions["pluralise"][2]("bottle", N);
                console.log(functions["count"][1](N) + " " + Bot + " of beer on the wall, " + functions["count"][1](N) + " " + 
                        Bot + " of beer\n");
            }
        },

        versetwo: {
            1: function(N) {
                if (N < 0) {
                    console.log("Go to the store and buy some more, 99 bottles of beer on the wall\n\n");
                }
                else {
                    console.log("Take one down and pass it around, " + functions["count"][1](N) + " " + 
                            functions["pluralise"][2]("bottle", functions["count"][1](N)) + " of beer on the wall\n\n");
                }
            } 
        },

        count: {
            1: function(N) {
                if (N === 0) {
                    return "No more";
                }
                else {
                    return N.toString();
                }
            }
        },

        pluralise: {
            2: function(Word, N) {
                if (N === 1) {
                    return Word;
                }
                else {
                    return Word + "s";
                }
            }
        }
    }

    return exports;
} )()
