$(function() {

  var lastNavElem;

  //load initial page based on the url
  //fragment if one exists
  var defaultPage = 'home';
  var validPages = [
    'home',
    'using',
    'other',
    'authors'
  ];

  if (window.location.hash) {
    var page = window.location.hash.substr(1);
  }

  if ($.inArray(page, validPages) == -1) {
    page = defaultPage;
  }

  var pageUrl = 'pages/' + page + '.html'
  var currNavElem = $(".page[href='#" + page + "']");
  lastNavElem = handlePageClick(pageUrl, currNavElem, lastNavElem);

  //event listeners
  $('.page').on('click', function() {
    var el = $(this);
    var url = el.data('url');
    lastNavElem = handlePageClick(url, el, lastNavElem);
  });
  $('.chapter').on('click', function() {
    var el = $(this);
    var url = el.data('url');
    lastNavElem = handlePageClick(url, el, lastNavElem);
  });
});


/**
 * Handle the page click
 *
 * @param {string} url
 * @param {Element} currElem
 * @param {?Element} lastElem
 * @return {Element}
 */
function handlePageClick(url, currElem, lastElem) {
  $('.page-content').load(url);
  if (lastElem) {
    lastElem.removeClass('selected');
  }
  currElem.addClass('selected');

  //if this nav elem is in a dropdown, then we also
  //need to highlight its parent
  var currParent = currElem.closest('li.dropdown');
  if (currParent && currParent.length) {
    currParent.children().first().addClass('selected');
  }
  if (lastElem) {
    var lastParent = lastElem.closest('li.dropdown');
    if (lastParent && lastParent.length) {
      lastParent.children().first().removeClass('selected');
    }
  }

  return currElem;
}
