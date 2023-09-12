## Chat

A module for generating and posting select chat messages.

Note: For `buildChatMessageContent` and `postChatMessage`, `data` represents an `object` containing all the data required to render the chat message template being used, including the template name itself. So, at a bare minimum, it should always look like this:

```javascript
let data = {
  template: "<template-name>"
};
```

This template name will be the name of the file (`systems/mythic.templates/chat/<template-name>.hbs`). You can inspect the templates to see what data they required in order to construct this object. If you're using your own templates, you must provide the data required for those, as well as modifying the path to the template from what's in the example given here.

```javascript
/**
 * Build the content for a chat message.
 *
 * @async
 * @param {object} data - The data that will be passed to the Handlebars
 * template.
 * @returns {string} The raw HTML of the chat message to be posted.
 */
async function buildChatMessageContent(data) { ... }

/**
 * Builds the i tag holding the scatter direction arrow.
 *
 * @param {number} roll - The roll result for the scatter direction.
 * @returns {string} The HTML i tag with the rotated scatter arrow.
 */
function getScatterArrow(roll) { .. }

/*
 * Provides the localization path for postable item chat flavor.
 *
 * @param {object} item - The Item that is being rolled to chat.
 * @returns {string} The localization path.
 */
function getPostableItemFlavorPath(item) { .. }

/*
 * Post a message to chat.
 *
 * @async
 * @param {object} data - The data that will be passed to the Handlebars
 * template.
 * @param {object} actor - The Actor that is rolling the message to chat.
 */
async function postChatMessage(data, actor) { .. }
```
