
function isLatin1Char(c) {
    if (typeof c == "string") {
        c = c.charCodeAt(0);
    }
    return (c >= 32 && c <= 126) || (c >= 160 && c <= 255);
}


if (typeof exports != "undefined") {
    exports.isLatin1Char = isLatin1Char;
}
