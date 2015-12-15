$(function() {

  var lastNavElem;

  //load initial page based on the url
  //fragment if one exists
  var validPages = [
    'home',
    'using',
    'other',
    'authors'
  ];
  var hash = '#home';
  if (window.location.hash) {
    hash = window.location.hash;
  }
  hash = hash.substr(1, hash.length);
  if ($.inArray(hash, validPages) > -1) {
    var pageUrl = 'pages/' + hash + '.html'
    var currNavElem = $(".page[href='#" + hash + "']");
    handlePageClick(pageUrl, currNavElem, lastNavElem);
  }

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
