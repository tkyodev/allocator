# Allocator

To create a new page

* Click "Create"
* Type a title
* Click the "+" dropdown and select "Other macros"
* Search for "html" and select "HTML" macro ("Allow the use of HTML code within a page.")
* Copy and paste the HTML below in the macro
* Inspect the macro (Right-click, "Inspect") and copy the "data-macro-id" value from the table
* Replace "ADD-MACRO-ID-HERE" with such value
* Save

```html
<allocator-app></allocator-app>
<script>dataMacroId = "ADD-MACRO-ID-HERE"</script>
<script src="https://tkyodev.github.io/allocator/allocator.min.js"></script>
```

Using the generic URL `https://tkyodev.github.io/allocator/allocator.min.js` will use the latest version.

If you want to use some specific version, use the versioned URL, for example `https://tkyodev.github.io/allocator/allocator.0.1.min.js`.