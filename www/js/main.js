function ready(fn) {
    if (document.readyState != 'loading') {
	fn();
    } else {
	document.addEventListener('DOMContentLoaded', fn);
    }
}

ready(function () {
    var inputs = document.querySelectorAll('.datepicker');
    [].forEach.call(inputs, function (input) {
	rome(input, {
	    inputFormat: "YYYY-MM-DDTHH:mm:ss",
	    weekStart: 1,
	    min: input.dataset.min,
	    max: input.dataset.max
	});
    });
});
