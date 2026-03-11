module.exports.print_error = function(m) {
    if (m.errors) {
        for (let i = 0; i < m.errors.length; i++) {
            if (typeof m.errors[i] === "string" && i % 2 == 1) {
                console.log(m.errors[i])
            } else if (typeof m.errors[i] === "object") {
                console.log("Error object:")
                console.log(JSON.stringify(m.errors[i]))
            }
        }
    }
};

module.exports.print_result = function(m) {
    console.log(m.result)
};


module.exports.print_warnings = function(m){
    if (m.warnings){
        for (w of m.warnings){
            if (typeof w === "string"){
                console.log(w)
            } else {
                console.log("Warning object:")
                console.log(JSON.stringify(w))
            }
        }
    }
}
