/*
 - Description
 	Plugin for jQuery. Populate html with a json data source.
 - Author
 	Alexandre Caprais
 - Licence
 	GNU Lesser General Public License
*/

jQuery.fn.fill = function(obj, options) {
	function browseJSON(obj, element) {
		// prepare
		var path = path || '';
		// no object
		if(obj == undefined) {
		}
		// branch
		else if (obj.constructor == Object) {
			for(var prop in obj){
				var child = jQuery.makeArray(jQuery("."+prop, element)).length > 0 ? jQuery("."+prop, element) : jQuery("#"+prop, element);
				browseJSON(obj[prop], jQuery(child, element));
			}
		}
		// array
		else if(obj.constructor == Array) {
			var arr = jQuery.makeArray(element);
			//how many duplicate
			var nbToCreate = obj.length - arr.length;
			var i = 0;
			for(iExist = 0; iExist < arr.length; iExist++) {
				if(i < obj.length) {
					$(element).eq(iExist).fill(obj[i]);
				}
				i++;
			}
			//fill started by last
			i = obj.length - 1;
			for(iCreate = 0; iCreate < nbToCreate; iCreate++) {
				//duplicate the last
				$(arr[arr.length - 1]).clone(true).insertAfter(arr[arr.length - 1]).fill(obj[i]);
				i--;
			}
		}
		// data only
		else {
			$(element).html(obj);
		}
	}
	
	this.each(function() {
		browseJSON(obj, this);
	});
return this;
}
