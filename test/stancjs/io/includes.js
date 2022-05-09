var stanc = require("../../../src/stancjs/stancjs.bc.js");
var utils = require("../utils/utils.js");


let include_model = stanc.stanc_io("basic.stan", ["auto-format", "canonicalize=includes"]);
utils.print_result(include_model)
