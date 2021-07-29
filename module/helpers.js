/** @module helpers */

/**
 * Register Handlebars helper to concatenate strings.
 * @param {...string} var_args - The strings to be concatenated.
 * @returns {string} The concatenated string.
 */
Handlebars.registerHelper("concat", function() {
  let str = "";
  for(var arg in arguments){
    if(typeof arguments[arg] != "object"){
      str += arguments[arg];
    }
  }
  return str;
});

/**
 * Register Handlebars helper to perform comparisons.
 * @param {string} operator - A string representation of a comparison operator.
 * @param {boolean} v1 - The outcome of a boolean expression evaluated by Handlebars.
 * @param {boolean} v2 - The outcome of a boolean expression evaluated by Handlebars.
 * @returns {boolean} The outcome of the comparison operation.
 */
Handlebars.registerHelper('cond', function(operator, v1, v2) {
  switch (operator) {
    case '==': return (v1 == v2);
    case '===': return (v1 === v2);
    case '!=': return (v1 != v2);
    case '!==': return (v1 !== v2);
    case '<': return (v1 < v2);
    case '<=': return (v1 <= v2);
    case '>': return (v1 > v2);
    case '>=': return (v1 >= v2);
    case '&&': return (v1 && v2);
    case '||': return (v1 || v2);
    default: return options.inverse(this);
  }
});

/**
 * Translated a number into a string representation of that number in the local format.
 * @param {number} num - The number to be converted.
 * @returns {string} The string representing the provided number in local format.
 */
Handlebars.registerHelper("localnum", function(num) {
  return num.toLocaleString();
});

/**
 * Invert the given boolean value.
 * @param {boolean} - The boolean value to invert.
 * @returns {boolean} The inverted boolean value.
 */
Handlebars.registerHelper("not", function(arg) {
  return !arg;
});

/**
 * Repeat a section of code n times.
 * @param {number} n - The number of times to repeat the code block.
 * @param {Block} block - The Handlebars block.
 * @returns {string} A string of the repeated HTML code.
 */
Handlebars.registerHelper("times", function(n, block) {
  let accum = "";
  for (let i = 0; i < n; i++) accum += block.fn(i);
  return accum;
});