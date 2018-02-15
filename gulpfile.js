var gulp   = require("gulp"),
    concat = require("gulp-concat"),
    rename = require("gulp-rename"),
    uglify = require("gulp-uglify"),
    babel  = require("gulp-babel"),
    lint   = require("gulp-jshint");

var js_files = ["src/prefabs/jarlang_rts.js",
                "src/prefabs/module_erlang.js",
                "src/prefabs/module_io.js",
                "src/prefabs/datatype_atom.js",
                "src/prefabs/datatype_bitstring.js",
                "src/prefabs/datatype_fun.js",
                "src/prefabs/datatype_list.js",
                "src/prefabs/datatype_map.js",
                "src/prefabs/datatype_number.js",
                "src/prefabs/datatype_pid.js",
                "src/prefabs/datatype_port.js",
                "src/prefabs/datatype_process.js",
                "src/prefabs/datatype_reference.js",
                "src/prefabs/datatype_tuple.js",
                "src/prefabs/datatype_unbound.js",
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