'use strict';
const lambda = require("./node_modules/Main");

exports.handler = function(event, context, callback) {
    callback(null, lambda.handler(context)(event));
};
