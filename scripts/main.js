// jQuery
// Document ready
$(function() {
  var basepath = $('body').data('basepath') || '';
  // highlight the selected nav item
  var pathname = window.location.pathname;
  // strip trailing slash
  pathname = pathname.replace(/\/$/,'');
  if (!pathname.length || pathname == basepath) {
    pathname = basepath + '/pages/home';
  }
  $('a.page').each(function(index) {
    var el = $(this);
    if (el.attr('href') == pathname) {
      el.addClass('selected');
      // highlight its parent nav item if it has one
      var closest = el.closest('li.dropdown');
      if (closest) {
        closest.children().first().addClass('selected');
      }
    }
  });
});
