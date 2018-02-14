var gulp = require("gulp"),
    concat = require("gulp-concat"),
    rename = require("gulp-rename"),
    uglify = require("gulp-uglify");

var js_files = ["src/prefabs/runtime.js",
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

gulp.task("default", function() {
    return gulp.src(js_files)
           .pipe(concat("jarlang.js"))
           .pipe(gulp.dest("gulpbuild/"));
});