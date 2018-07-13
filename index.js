'use strict';
const lambda = require("./node_modules/Main");

exports.handler = function(event, context) {
    return lambda.handler(context)(event)();
};
