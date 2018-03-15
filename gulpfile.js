var gulp   = require("gulp"),
    concat = require("gulp-concat"),
    rename = require("gulp-rename"),
    uglify = require("gulp-uglify"),
    babel  = require("gulp-babel"),
    lint   = require("gulp-jshint");

var js_files = [// Jarlang Runtime
                "src/js/runtime.js",

                // Jarlang Class Definitions
                "src/js/classes/datatype_erlangDatatype.js",
                "src/js/classes/datatype_atom.js",
                "src/js/classes/datatype_bitstring.js",
                "src/js/classes/datatype_float.js",
                "src/js/classes/datatype_fun.js",
                "src/js/classes/datatype_int.js",
                "src/js/classes/datatype_list.js",
                "src/js/classes/datatype_map.js",
                "src/js/classes/datatype_pid.js",
                "src/js/classes/datatype_port.js",
                "src/js/classes/datatype_process.js",
                "src/js/classes/datatype_reference.js",
                "src/js/classes/datatype_tuple.js",
                "src/js/classes/datatype_unbound.js",

                // Jarlang Prefab Modules
                "src/js/modules/module_erlang.js",
                "src/js/modules/module_io.js",
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
