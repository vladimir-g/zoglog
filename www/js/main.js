function ready(fn) {
    if (document.readyState != 'loading') {
	fn();
    } else {
	document.addEventListener('DOMContentLoaded', fn);
    }
}

function getOffset(el) {
    var rect = el.getBoundingClientRect()
    var bodyEl = document.documentElement || document.body;

    return {
	top: rect.top + bodyEl.scrollTop,
	left: rect.left + bodyEl.scrollLeft
    }
}

ready(function () {
    // Initialize rome.js
    var inputs = document.querySelectorAll('.datepicker');
    [].forEach.call(inputs, function (input) {
	rome(input, {
	    inputFormat: "YYYY-MM-DDTHH:mm:ss",
	    weekStart: 1,
	    min: input.dataset.min,
	    max: input.dataset.max
	});
    });

    // Show in context
    if (window.location.hash.indexOf('#msg-') === 0) {
	var row = document.querySelector(window.location.hash);
	if (row !== null) {
	    row.className = 'success';
	    // Move viewport to get row into view
	    var y = getOffset(row).top - 200;
	    if (y < 0)
		y = 0;
	    document.documentElement.scrollTop = y;
	    document.body.scrollTop = y;
	}
    }

    // Submit form without empty values
    var filterForm = document.getElementById('filter-form');
    filterForm.addEventListener('submit', function (event) {
	event.preventDefault();
	var query = [];
	[].forEach.call(filterForm.querySelectorAll('input'), function (el) {
	    var val = el.value.trim();
	    if (val.trim()) {
		query.push(encodeURIComponent(el.name) +
			   '=' +
			   encodeURIComponent(val));
	    }
	});
	window.location.href = filterForm.action + query.join('&');
    });

});
