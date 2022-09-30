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

function add_person_entry(name) {
    let container = document.getElementsByClassName(`js-${name}s`)[0];
    let position = container.getElementsByClassName("js-insert-before")[0];
    let input = document.createElement("input");
    input.setAttribute("class", "form-input form-input--inline");
    input.setAttribute("type", "text");
    input.setAttribute("name", `${name}[]`);
    input.setAttribute("list", `${name}s`);
    container.insertBefore(input, position);
    container.insertBefore(document.createTextNode(" "), position);
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
    input_location.setAttribute("class", "form-input form-input--narrow");
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
