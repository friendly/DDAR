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
  handlePageClick(pageUrl, currNavElem, lastNavElem);

  //event listeners
  $('.page').on('click', function() {
    var el = $(this);
    var url = el.data('url');
    handlePageClick(url, el, lastNavElem);
  });
  $('.chapter').on('click', function() {
    var el = $(this);
    var url = el.data('url');
    handlePageClick(url, el, lastNavElem);
  });
});


// helper functions
function handlePageClick(url, currElem, lastElem) {
  $('.page-content').load(url);
  if (lastElem) {
    lastElem.removeClass('selected');
  }
  currElem.addClass('selected');

  //special case for content nav element which is a
  //parent and should also be highlighted
  var chaptersNavElem = $('.content-page');
  if (url.indexOf('chapters') > -1) {
    chaptersNavElem.addClass('selected');
  } else {
    chaptersNavElem.removeClass('selected');
  }
  lastElem = currElem;
}
