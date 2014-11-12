$(function () {

  var $window = $(window);

  var $menuButton = $("nav h2");

  $menuButton.on("click", function () {
    var $body = $(document.body);
    $body.toggleClass("show-nav");
    $menuButton.text($body.is(".show-nav") ? "Close" : "Menu");
  });

  // --------------------------------------------------------------------------

  var $featureList = $("section.features ul");
  var $more = $("<li class=\"moreless\">More&hellip;</li>");

  $more.on("click", function () {
    $featureList.toggleClass("expanded");
    $more.html($featureList.is(".expanded") ? "&hellip;Less" : "More&hellip;");
    // TODO: scroll to top of feature list if offscreen after collapse
  });

  $featureList.append($more);

  // --------------------------------------------------------------------------

  var $examples = $("section.example div.example");

  $("section.example div.example pre")
    .before("<div class='nav'>" +
             "  <button class='prev'>Previous</a>" +
             "  <button class='next'>Next</a>" +
             "</div>");

  var go = function (delta) {
    return function (e) {
      var index = $examples.filter(".current").index();
      var nextIndex = (index - 1 + delta) % $examples.length;
      $examples.removeClass("current");
      $examples.eq(nextIndex).addClass("current");
      e.preventDefault();
    };
  };

  $("section.example button.prev").on("click", go(-1));
  $("section.example button.next").on("click", go(1));

});
