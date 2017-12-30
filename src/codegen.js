var escodegen = require("escodegen");
var fs = require("fs");

const args = process.argv.slice(2);

const generate = function(input) {
    // Try parsing data as JSON
    try {
        data = JSON.parse(input);
    } catch (e) {
        console.log("Error passing file data into escodegen. Please ensure input is valid JSON");
        console.log("Input: ");
        console.log(input);
        console.log("Error: ");
        console.log(e);
        return false;
    }

    return console.log(escodegen.generate(data));
}

if (!args.length) {
    console.log("Please supply at least one argument to this script. All arguments must be filepaths");
    return false;
}

args.forEach(arg => {
    let isFilepath = !!fs.existsSync(arg);
    let input = "";

    // If given argument is a filepath, read contents of file to pass into escodegen.generate, otherwise
    // arguments will be taken as string input instead
    if (isFilepath) {
        fs.readFile(arg, "utf-8", function(err, data) {
            if (err) {
                console.log("File reading ran into the following error:");
                console.log(err);
                console.log("Aborting...");
                return false;
            }

            console.log(`Attempting JavaScript code generation...\n`);
            generate(data);
        });
    }
    else {
        console.log(`'${arg}' is not a valid file, skipping...`);
    }
});

