// TODO:
// - js2-mode
// - monospace font

user_pref("extensions.checkCompatibility", false);

interactive("fill-domain", "Fill the minibuffer with the current domain.",
  function (I) {
	      var field = I.minibuffer.input_element;
	      var paths = String(I.window.content.location).split('/');
	      var domain = paths[0] + "/" + paths[1] + "/" + paths[2] + "/";
	      field.value = domain;
});

interactive("tinyurl", "Create a TinyURL for the current URL",
  function(I) {
    I.window.content.location.href = 'http://tinyurl.com/create.php?url=' +
        encodeURIComponent(I.window.content.location.href);
});

define_key(minibuffer_keymap, kbd("/", MOD_CTRL), "fill-domain");

add_webjump("hub", "http://github.com/search?q=%s");
add_webjump("wikipedia", "http://www.google.com/search?q=wikipedia+%s&btnI=I'm Feeling Lucky");
add_delicious_webjumps ("rotty");

url_remoting_fn = load_url_in_new_buffer;
url_completion_use_history = true;
url_completion_use_bookmarks = true;
can_kill_last_buffer = false;

register_user_stylesheet(
    "data:text/css,"+
        escape("#minibuffer, tree.completions, .mode-line { font-family: Inconsolata; font-size: 12pt; }"));

interactive("toggle-stylesheets",
            "Toggle whether conkeror uses style sheets (CSS) for the " +
            "current buffer.  It is sometimes useful to turn off style " +
            "sheets when the web site makes obnoxious choices.",
            function(I) {
  var s = I.buffer.document.styleSheets;
  for (var i = 0; i < s.length; i++)
    s[i].disabled = !s[i].disabled;
});

// Thanks; _why!
function resize_textarea_up(field) {
  var h = field.offsetHeight;
  if (h > 120)
    field.style.height = (h - 60) + "px";
}
function resize_textarea_down(field) {
  field.style.height = (parseInt(field.offsetHeight) + 60) + "px";
}
interactive(
  "resize-textarea-up",
  "Resize a textarea to be smaller.",
  function (I) { call_on_focused_field(I, resize_textarea_up); }
);
interactive(
  "resize-textarea-down",
  "Resize a textarea to be taller.",
  function (I) { call_on_focused_field(I, resize_textarea_down); }
);

define_key(content_buffer_textarea_keymap, "C-up", "resize-textarea-up", $category = "Movement");
define_key(content_buffer_textarea_keymap, "C-down", "resize-textarea-down", $category = "Movement");

restore_session();
