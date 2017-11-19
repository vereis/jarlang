// Constructor
function Map() {
    this.value = undefined;
}

// Static Methods
Map.isMap = (map) => map instanceof Map;

// Prototype Methods
Map.prototype.value = function() {
    throw "Map is not implemented yet";
}

Map.prototype.toString = function() {
    throw "Map is not implemented yet";
}

Map.prototype.isUnbound = function() {
    return false;
}

Map.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (Map.isMap(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
}