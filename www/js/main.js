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

// Very simple object merging
function mergeObj() {
    if (arguments.length === 0) {
        return null;
    }
    var result = arguments[0];
    for (var i = 1; i < arguments.length; i++) {
        for (var key in arguments[i]) {
            if (arguments[i].hasOwnProperty(key))
                result[key] = arguments[i][key];
        }
    }
    return result;
}

ready(function () {

    // Initialize rome.js
    var dateConf = {
	inputFormat: "YYYY-MM-DDTHH:mm:ss",
	weekStart: 1
    };
    var dateFrom = document.getElementById('date-from');
    var dateTo = document.getElementById('date-to');
    var skipTo = document.getElementById('skip-to-date');

    rome(dateFrom, mergeObj({
        dateValidator: rome.val.beforeEq(dateTo)
    }, dateConf));

    rome(dateTo, mergeObj({
        dateValidator: rome.val.afterEq(dateFrom)
    }, dateConf));

    var skipTo = document.querySelectorAll('.skip-to-date')
    for (var i = 0; i < skipTo.length; i++) {
        rome(skipTo[i], mergeObj({
            min: skipTo[i].dataset.min,
            max: skipTo[i].dataset.max
        }, dateConf));
    }

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

    // AJAX nickname autocomplete
    var nicksLoading = false;
    var nicksLoaded = false;
    var nickInput = document.getElementById('nick');
    var nickIcon = document.getElementById('nick-icon');
    var dataList = document.createElement('datalist');
    dataList.id = 'nicks';
    nickInput.addEventListener('focus', function (e) {
        if (nicksLoading || nicksLoaded)
            return;
        nicksLoading = true;
        // Show icon
        nickIcon.classList.remove('glyphicon-user');
        nickIcon.classList.add('glyphicon-refresh');
        nickIcon.classList.add('spinning');
         // Load nick list from server
        var req = new XMLHttpRequest();
        var server = encodeURIComponent(nickInput.dataset.server);
        var channel = encodeURIComponent(nickInput.dataset.channel);
        req.open('GET',
                 '/nicknames/?server=' + server + '&channel=' + channel);
        req.addEventListener('load', function () {
            // Check errors
            if (this.status !== 200)
                return;
            // Populate datalist
            this.responseText.split('\n').forEach(function (nick) {
                var opt = document.createElement('option');
                opt.textContent = nick;
                opt.value = nick;
                dataList.appendChild(opt);
            });
            // Append datalist to input
            nickInput.parentNode.appendChild(dataList);
            nickInput.setAttribute('list', 'nicks');
            nicksLoaded = true;
        });
        // Hide icon when request is finished
        req.addEventListener('loadend', function () {
            nicksLoading = false;
            // Hide icon
            nickIcon.classList.remove('spinning');
            nickIcon.classList.remove('glyphicon-refresh');
            nickIcon.classList.add('glyphicon-user');
        });
        req.send();
    });

});
