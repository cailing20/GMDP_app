(function () {
  function initializeDashboard() {
    var dashboard = document.getElementById("gmdp-dashboard");
    if (!dashboard) return;

    dashboard.querySelectorAll(".bslib-full-screen-enter").forEach(function (button) {
      var title = button.closest(".card").querySelector(".card-header").textContent.trim();
      button.setAttribute("role", "button");
      button.setAttribute("aria-label", "Expand " + title.toLowerCase());
      button.addEventListener("keydown", function (event) {
        if (event.key === "Enter" || event.key === " ") {
          event.preventDefault();
          button.click();
        }
      });
    });

    var pending = false;
    function fitDashboard() {
      if (pending) return;
      pending = true;
      window.requestAnimationFrame(function () {
        pending = false;
        if (!dashboard.getClientRects().length) return;
        // Measure the actual top edge: the navigation can wrap onto two lines.
        var top = dashboard.getBoundingClientRect().top + window.scrollY;
        var height = Math.max(0, window.innerHeight - top - 16);
        dashboard.style.setProperty("--gmdp-dashboard-height", height + "px");
      });
    }

    window.addEventListener("resize", fitDashboard);
    if (window.ResizeObserver) {
      var navbar = document.querySelector("nav.navbar");
      if (navbar) new ResizeObserver(fitDashboard).observe(navbar);
    }
    if (window.jQuery) window.jQuery(document).on("shown.bs.tab", fitDashboard);
    if (document.fonts) document.fonts.ready.then(fitDashboard);
    fitDashboard();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeDashboard);
  } else {
    initializeDashboard();
  }
})();
