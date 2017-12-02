// Constructor
function ErlMap(map) {
    this.value = typeof map == "object" ? map : {};
}

// Static Methods
ErlMap.isErlMap = (map) => map instanceof ErlMap;

// Prototype Methods
ErlMap.prototype.getValue = function() {
    return this.value;
};

ErlMap.prototype.get = function(key) {
    if (typeof key != "string") {
        key = JSON.stringify(key);
    }

    return this.value[key];
};

ErlMap.prototype.put = function(key, value) {
    if (typeof key != "string") {
        key = JSON.stringify(key);
    }

    this.value[key] = value;
};

ErlMap.prototype.update = function(key, value) {
    if (typeof key != "string") {
        key = JSON.stringify(key);
    }

    if (this.value[key]) {
        this.value[key] = value;
    }
};

ErlMap.prototype.remove = function(key) {
    if (typeof key != "string") {
        key = JSON.stringify(key);
    }

    delete this.value[key];
};

ErlMap.prototype.size = function() {
    var size = 0, k;

    for (k in this.value) {
        if (this.value.hasOwnProperty(k)) {
            size++;
        }
    }

    return size;
};

ErlMap.prototype.toString = function() {
    var pairs = [], k;

    for (k in this.value) {
        if (this.value.hasOwnProperty(k)) {
            pairs.push(k + "=>" + JSON.stringify(this.value[k]));
        }
    }

    return "#{" + pairs.join(",") + "}";
};

ErlMap.prototype.typeOf = function() {
    return "ErlMap";
};

ErlMap.prototype.isUnbound = function() {
    return false;
};

ErlMap.prototype.match = function(X) {
    // Since 'false' is a legitimate atom in Erlang, we return undefined instead of false for a failure case
    if (ErlMap.isErlMap(X) && this.value === X.value) {
        return X;
    }
    else {
        return undefined;
    }
};

if (typeof exports != "undefined") {
    exports.ErlMap = ErlMap;
}
