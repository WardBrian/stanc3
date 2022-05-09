var stanc = require("../../../src/stancjs/stancjs.bc.js");
var utils = require("../utils/utils.js");


let basic = stanc.stanc_io("basic.stan");
utils.print_error(basic)
