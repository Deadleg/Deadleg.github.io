// Affix the sidebar to make it static
$(function() {
    $('#sidebar').affix();
});

// Set the sidebar width
$(function() {
    var colWidth = $('.col-md-4').width();
    $('#sidebar').width(colWidth);
});

// Change the active state in the nav-bar
$(function() {
    var path = location.pathname
    if (path == '/') {
        $('#navbar-buttons').find('a[href="/"]').parents('li').addClass('active');
    } else {
        $('#navbar-buttons a').each(function() { 
            if ($(this).attr('href') != '/' && path.indexOf($(this).attr('href')) > -1) {
                $(this).parents('li').addClass('active');
            } else if ($(this).attr('href') != '/' && path.substring(1,5).indexOf("20") > -1 && $(this).attr('href') == "/blog/") {
                console.log(path.substring(1,5));
                $(this).parents('li').addClass('active');
            }
        });
    }
});