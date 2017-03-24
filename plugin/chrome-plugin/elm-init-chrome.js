document.addEventListener('DOMContentLoaded', function() {
  var app = Elm.Main;
  var div = document.getElementById('brain-plugin');

  app.embed(div);
  chrome.tabs.onActived.addListener(tabActivated);
  chrome.tabs.onUpdated.addListener(tabUpdated);

  function tabActivated(id, change) {
    if (change.url) {
      app.ports.tabActivated(change.url);
    };
  };

  function tabUpdated(id, change) {
    if (getCurrentPageUrl() == change.url) {
      app.ports.tabUpdated(change.url);
    };
  };

  function getCurrentPageUrl() {
    var ct = chrome.tabs.getCurrent();

    return (ct && ct.url) ? ct.url : '';
  }
});
