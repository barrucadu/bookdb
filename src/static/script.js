/* derived from http://cssglobe.com/lab/tooltip/02/ */
function set_preview_hover(el) {
    const xOffset = 10;
    const yOffset = 30;
    const body = document.getElementsByTagName("body")[0];

    el.onmouseenter = function(event) {
        let preview = document.createElement("img");
        preview.setAttribute("id", "preview");
        preview.setAttribute("src", el.href);
        preview.setAttribute("alt", el.title);
        preview.setAttribute("style", `top:${event.pageY + xOffset}px; left:${event.pageX + yOffset}px;`);
        body.appendChild(preview);
    }

    el.onmouseleave = function() {
        let preview = document.getElementById("preview");
        body.removeChild(preview);
    }

    el.onmousemove = function(event) {
        let preview = document.getElementById("preview");
        preview.setAttribute("style", `top:${event.pageY + xOffset}px; left:${event.pageX + yOffset}px;`);
    }
}

/* derived from https://www.w3schools.com/howto/howto_js_autocomplete.asp */
function autocomplete(inp, arr) {
  /*the autocomplete function takes two arguments,
  the text field element and an array of possible autocompleted values:*/
  var currentFocus;
  /*execute a function when someone writes in the text field:*/
  inp.addEventListener("input", function(e) {
      let a, b, i, val = this.value;
      /*close any already open lists of autocompleted values*/
      closeAllLists();
      if (!val) { return false;}
      let tval = val.toUpperCase().replace(/[^\w\s]/gi, '');
      currentFocus = -1;
      /*create a DIV element that will contain the items (values):*/
      a = document.createElement("DIV");
      a.setAttribute("id", this.id + "autocomplete-list");
      a.setAttribute("class", "autocomplete-items");
      /*append the DIV element as a child of the autocomplete container:*/
      this.parentNode.appendChild(a);
      /*for each item in the array...*/
      for (i = 0; i < arr.length; i++) {
        /*check if the item includes the text field value:*/
        if (arr[i].toUpperCase().replace(/[^\w\s]/gi, '').includes(tval)) {
          /*create a DIV element for each matching element:*/
          b = document.createElement("DIV");
          /*make the matching letters bold:*/
          b.innerHTML += arr[i];
          /*insert a input field that will hold the current array item's value:*/
          b.innerHTML += "<input type='hidden' value='" + arr[i] + "'>";
          /*execute a function when someone clicks on the item value (DIV element):*/
          b.addEventListener("click", function(e) {
              /*insert the value for the autocomplete text field:*/
              inp.value = this.getElementsByTagName("input")[0].value;
              /*close the list of autocompleted values,
              (or any other open lists of autocompleted values:*/
              closeAllLists();
          });
          a.appendChild(b);
        }
      }
  });
  /*execute a function presses a key on the keyboard:*/
  inp.addEventListener("keydown", function(e) {
      if(document.getElementsByClassName("autocomplete-items").length == 0) { return; }

      let x = document.getElementById(this.id + "autocomplete-list");
      if (x) x = x.getElementsByTagName("div");
      if (e.keyCode == 40) {
        /*If the arrow DOWN key is pressed,
        increase the currentFocus variable:*/
        currentFocus++;
        /*and and make the current item more visible:*/
        addActive(x);
      } else if (e.keyCode == 38) { //up
        /*If the arrow UP key is pressed,
        decrease the currentFocus variable:*/
        currentFocus--;
        /*and and make the current item more visible:*/
        addActive(x);
      } else if (e.keyCode == 13) {
        /*If the ENTER key is pressed, prevent the form from being submitted,*/
        e.preventDefault();
        if (currentFocus > -1) {
          /*and simulate a click on the "active" item:*/
          if (x) x[currentFocus].click();
        }
      } else if (e.keyCode == 9) {
        closeAllLists();
      }
  });
  function addActive(x) {
    /*a function to classify an item as "active":*/
    if (!x) return false;
    /*start by removing the "active" class on all items:*/
    removeActive(x);
    if (currentFocus >= x.length) currentFocus = 0;
    if (currentFocus < 0) currentFocus = (x.length - 1);
    /*add class "autocomplete-active":*/
    x[currentFocus].classList.add("autocomplete-active");
  }
  function removeActive(x) {
    /*a function to remove the "active" class from all autocomplete items:*/
    for (let i = 0; i < x.length; i++) {
      x[i].classList.remove("autocomplete-active");
    }
  }
  function closeAllLists(elmnt) {
    /*close all autocomplete lists in the document,
    except the one passed as an argument:*/
    let x = document.getElementsByClassName("autocomplete-items");
    for (let i = 0; i < x.length; i++) {
      if (elmnt != x[i] && elmnt != inp) {
        x[i].parentNode.removeChild(x[i]);
      }
    }
  }
  /*execute a function when someone clicks in the document:*/
  document.addEventListener("click", function (e) {
      closeAllLists(e.target);
  });
}

function add_person_entry(container_clazz, name, entries) {
    let container = document.getElementsByClassName(container_clazz)[0];
    let position = container.getElementsByClassName("js-insert-before")[0];
    let el = document.createElement("div");
    let input = document.createElement("input");
    el.setAttribute("class", "autocomplete");
    input.setAttribute("class", "form-input");
    input.setAttribute("type", "text");
    input.setAttribute("name", name);
    el.appendChild(input);
    container.insertBefore(el, position);
    container.insertBefore(document.createTextNode(" "), position);
    autocomplete(input, entries);
}

var location_count = 0;

function add_holding_entry(n, locations) {
    location_count += 1;
    let nth = n+location_count;

    let container = document.getElementsByClassName("js-holdings")[0];
    let position = container.getElementsByClassName("js-insert-before")[0];
    let row = document.createElement("div");
    row.setAttribute("class", "form-row");
    let group_location = document.createElement("div");
    group_location.setAttribute("class", "form-group");
    let group_notes = document.createElement("div");
    group_notes.setAttribute("class", "form-group");
    row.appendChild(group_location);
    row.appendChild(group_notes);
    let label_location = document.createElement("location");
    label_location.setAttribute("class", "form-label");
    label_location.setAttribute("for", `location_${nth}`);
    label_location.innerText = `Location ${nth}`;
    group_location.appendChild(label_location);
    let label_notes = document.createElement("location");
    label_notes.setAttribute("class", "form-label");
    label_notes.setAttribute("for", `notes_${nth}`);
    label_notes.innerText = "Notes";
    group_notes.appendChild(label_notes);
    let input_location = document.createElement("select");
    input_location.setAttribute("class", "form-input form-input-narrow");
    input_location.setAttribute("id", `location_${nth}`);
    input_location.setAttribute("name", "location[]");
    let option = document.createElement("option");
    option.value = "";
    option.selected = "";
    input_location.add(option);
    locations.forEach(function(loc) {
        let option = document.createElement("option");
        option.value = loc.key;
        option.text = loc.label;
        input_location.add(option);
    });
    group_location.appendChild(input_location);
    let input_notes = document.createElement("input");
    input_notes.setAttribute("class", "form-input");
    input_notes.setAttribute("id", `notes_${nth}`);
    input_notes.setAttribute("name", "notes[]");
    input_notes.setAttribute("type", "text");
    group_notes.appendChild(input_notes);
    container.insertBefore(row, position);
}
