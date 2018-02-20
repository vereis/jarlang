var gulp   = require("gulp"),
    concat = require("gulp-concat"),
    rename = require("gulp-rename"),
    uglify = require("gulp-uglify"),
    babel  = require("gulp-babel"),
    lint   = require("gulp-jshint");

var js_files = [// Jarlang Runtime
                "src/prefabs/runtime.js",

                // Jarlang Class Definitions
                "src/prefabs/classes/datatype_erlangDatatype.js",
                "src/prefabs/classes/datatype_atom.js",
                "src/prefabs/classes/datatype_bitstring.js",
                "src/prefabs/classes/datatype_float.js",
                "src/prefabs/classes/datatype_fun.js",
                "src/prefabs/classes/datatype_int.js",
                "src/prefabs/classes/datatype_list.js",
                "src/prefabs/classes/datatype_map.js",
                "src/prefabs/classes/datatype_pid.js",
                "src/prefabs/classes/datatype_port.js",
                "src/prefabs/classes/datatype_process.js",
                "src/prefabs/classes/datatype_reference.js",
                "src/prefabs/classes/datatype_tuple.js",
                "src/prefabs/classes/datatype_unbound.js",

                // Jarlang Prefab Modules
                "src/prefabs/modules/module_erlang.js",
                "src/prefabs/modules/module_io.js",
            ];

var js_dependencies = [
    "node_modules/bignumber.js/bignumber.js"
];

function build_rts(files, name) {
    return gulp.src(files !== undefined ? files : js_files.concat(js_dependencies))
               .pipe(concat(name || "jarlang.js"));
}

gulp.task("es6", function() {
    return build_rts()
           .pipe(gulp.dest("gulpbuild/"));
});

gulp.task("es5", function() {
    return build_rts()
           .pipe(babel())
           .pipe(gulp.dest("gulpbuild/"));
});

gulp.task("min", function() {
    return build_rts()
           .pipe(babel())
           .pipe(uglify())
           .pipe(gulp.dest("gulpbuild/"));
});

gulp.task("lint", function() {
    return gulp.src(js_files)
               .pipe(lint())
               .pipe(lint.reporter('default'));
});

// Same as min
gulp.task("default", function() {
    return build_rts()
           .pipe(babel())
           .pipe(uglify())
           .pipe(gulp.dest("gulpbuild/"));
});
