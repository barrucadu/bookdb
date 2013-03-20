<%
from datetime import date
%>

<%inherit file="wrapper.mako"/>

<form action="${target}" method="post">
  <fieldset>
    <ol>
      <li>
        <label for="isbn">ISBN</label>
        <input type="text" id="isbn" name="isbn" value="${isbn}" required>
      </li>
      <li>
        <label for="title">Title</label>
        <input type="text" id="title" name="title" value="${title}" required>
      </li>
      <li>
        <label for="isbn">Author</label>
        <input type="text" id="author" name="author" value="${author}" required>
      </li>
      <li>
        <label for="read">Read</label>
% if read:
        <input type="checkbox" id="read" name="read" checked>
% else:
        <input type="checkbox" id="read" name="read">
% endif
      </li>
      <li>
        <label for="lastread">Last Read</label>
% if lastread == date.min:
        <input type="date" id="lastread" name="lastread">
% else:
        <input type="date" id="lastread" name="lastread" value="${lastread}">
% endif
      </li>
      <li>
        <label for="location">Location</label>
        <input type="text" id="location" name="location" value="${location}" required>
      </li>
      <li>
        <label for="borrower">Borrower</label>
        <input type="text" id="borrower" name="borrower" value="${borrower}">
      </li>
      <li>
        <label for="quote">Quote</label>
        <textarea id="quote" name="quote">${quote}</textarea>
      </li>
      <li>
        <label for="notes">Notes</label>
        <textarea id="notes" name="notes">${notes}</textarea>
      </li>
      <li>
        <input type="submit" value="${submit}">
      </li>
    </ol>
  </fieldset>
</form>

<div class="help">
  <p>To enter multiple authors, separate them with an ampersand.</p>
</div>
