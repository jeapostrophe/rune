/*
  function parse_hash ( h ) {
  return h.substr(1).split(":");
  }

  $( document ).ready(function() {
  var h = window.location.hash;
  var hp = parse_hash ( h );

  if ( hp[0] == "bot" ) {
  var tid = 'row' + hp[1];
  var target = document.getElementById(tid);
  var dest = $(target).offset().top;
  console.log( "going to " + dest );
  // window.scrollTo( 0, dest );
  // $("body").animate({'scrollTop': dest}, 1);
  }

  });
*/
