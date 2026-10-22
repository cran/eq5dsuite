// Idle watch, used only in online mode.
//
// The server cannot tell a user reading a table from a user who has walked
// away, so the timer runs here and tells the server when it fires. Any real
// interaction resets it.
(function () {
  var warnAt = null, endAt = null, warned = false, timer = null;

  function reset() {
    if (warnAt === null) return;
    warned = false;
    clearTimeout(timer);
    schedule();
  }

  function schedule() {
    timer = setTimeout(function () {
      if (!warned) {
        warned = true;
        Shiny.setInputValue("eq5d_idle_warning", Date.now(), {priority: "event"});
        timer = setTimeout(function () {
          // Close the connection; Shiny shows its disconnected screen and the
          // server tears the session down.
          if (Shiny.shinyapp && Shiny.shinyapp.$socket) Shiny.shinyapp.$socket.close();
        }, (endAt - warnAt) * 1000);
      }
    }, warnAt * 1000);
  }

  // Registered once, when the script loads, as Shiny expects. They used to
  // be registered in a "shiny:connected" hook, and the reset handler took no
  // argument: Shiny refuses such a handler with an error, which stopped the
  // online app from loading data at all. A second registration, on a
  // reconnect, would have thrown too.
  var listening = false;
  Shiny.addCustomMessageHandler("eq5d_idle_start", function (m) {
    warnAt = m.warnAfter; endAt = m.endAfter;
    if (!listening) {
      listening = true;
      ["mousemove", "mousedown", "keydown", "touchstart", "scroll"]
        .forEach(function (ev) {
          document.addEventListener(ev, function () { if (!warned) reset(); },
                                    {passive: true});
        });
    }
    clearTimeout(timer);
    warned = false;
    schedule();
  });
  Shiny.addCustomMessageHandler("eq5d_idle_reset", function (m) { reset(); });
})();
