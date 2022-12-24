/** @module helpers */

/**
 * Register Handlebars helper to concatenate strings.
 *
 * @param {...string} arguments - The strings to be concatenated.
 * @returns {string} The concatenated string.
 */
Handlebars.registerHelper("concat", function() {
  let str = "";
  for (let arg in arguments) {
    if (typeof arguments[arg] != "object") {
      str += arguments[arg];
    }
  }
  return str;
});

/**
 *  Register Handlebars helper to check if a string is a substring of another
 *  string.
 *
 *  @param {string} string - A string to check for the presence of a substring
 *  in.
 *  @param {string} substring - The substring to check for in the main string.
 *  @returns {boolean} Whether or not the string contains the substring.
 */
Handlebars.registerHelper("contains", function(string, substring) {
  return string.includes(substring);
});

/**
 * Register Handlebars helper to perform comparisons.
 *
 * @param {string} operator - A string representation of a comparison operator.
 * @param {...*} var_args - An array of arguments containing a string
 * representation of the comparison operator, the booleans to be compared, and
 * optionally an object containing the HTML block from Handlebars.
 * @returns {boolean} The outcome of the comparison operations.
 */
Handlebars.registerHelper("cond", function(...var_args) {
  if (typeof(var_args.slice(-1)[0]) === "object") var_args.pop();
  const operator = var_args[0];
  return var_args.slice(1)
    .map(x => {if (x) { return true } else { return false }})
    .reduce(_compare);

  function _compare(v1, v2) {
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
      default: throw new Error(`The '${operator}' operator is not recognized.`);
    }
  }
});

/**
 * Register Handlebars helper to perform basic arithmetic operations.
 *
 * @param {string} operator - A string representation of the arithmetic
 * operator.
 * @param {number} v1 - The first number to be operated on.
 * @param {number} v2 - The second number to be operated on.
 * @returns {number} The result of the arithmetic operation.
 */
Handlebars.registerHelper("doMath", function(operator, v1, v2) {
  if (isNaN(v1)) throw new Error(`Argument "${v1}" is NaN.`);
  if (isNaN(v2)) throw new Error(`Argument "${v1}" is NaN.`);

  switch (operator) {
    case '+': return v1 + v2;
    case '-': return v1 - v2;
    case '*': return v1 * v2;
    case '/': return v1 / v2;
    default: throw new Error(`The '${operator}' operator is not recognized.`);
  }
});

/**
 * Retrieve a system setting.
 *
 * @param {string} setting - The name for the system setting to retrieve.
 * @returns {number, string} The system setting value.
 */
Handlebars.registerHelper("getSetting", function(setting) {
  return game.settings.get("mythic", setting);
});

/**
 * Determine if an object is "empty" - contains only false or null values.
 *
 * @param {object} data - The data object to check for emptiness.
 * @returns {boolean} Whehther or not the object is empty.
 */
Handlebars.registerHelper("isEmptyDataObject", function(data) {
  return Object.values(data).every(x => x === null || !x);
});

/**
 * Filter a list of Items to get only those of a requested type.
 *
 * @param {string} type - The type of item desired.
 * @param {Array.<object>} items - The list of Items.
 * @returns {Array.<object>} The list of Items matching the requested type.
 */
Handlebars.registerHelper("itemsOfType", function(type, items) {
  return items.filter(x => x.type === type);
});

/**
 * Translates a number into a string representation of that number in the local
 * format.
 *
 * @param {number} num - The number to be converted.
 * @returns {string} The string representing the provided number in local
 * format.
 */
Handlebars.registerHelper("localnum", function(num) {
  return num.toLocaleString();
});

/**
 * Invert the given boolean value.
 *
 * @param {boolean} - The boolean value to invert.
 * @returns {boolean} The inverted boolean value.
 */
Handlebars.registerHelper("not", function(arg) {
  if (arg) return !arg;
});

/**
 * Repeat a section of code n times.
 *
 * @param {number} n - The number of times to repeat the code block.
 * @param {Block} block - The Handlebars block.
 * @returns {string} A string of the repeated HTML code.
 */
Handlebars.registerHelper("times", function(n, block) {
  let accum = "";
  for (let i = 0; i < n; i++) accum += block.fn(i);
  return accum;
});
