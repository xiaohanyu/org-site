$(document).ready(function() {
    var pathname = window.location.href;
    $('a').each(function(){
       var link = $(this).attr('href');
       if (link.substr(0,1) == "#") {
           $(this).prop('href', pathname + link);
       }
    });
});