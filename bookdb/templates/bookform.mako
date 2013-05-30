<%
from datetime import date
%>

<%inherit file="wrapper.mako"/>

<form action="${webpath}/${target}" method="post" enctype="multipart/form-data">
  <fieldset>
    <ol>
      <li>
        <label for="cover">Cover Image</label>
        <input type="file" id="cover" name="cover">
      </li>
      <li>
        <label for="isbn" class="required">ISBN</label>
        <input type="text" id="isbn" name="isbn" value="${book.isbn}" required>
      </li>
      <li>
        <label for="title" class="required">Title</label>
        <input type="text" id="title" name="title" value="${book.title}" required>
      </li>
      <li>
        <label for="title">Subtitle</label>
        <input type="text" id="subtitle" name="subtitle" value="${book.subtitle}">
      </li>
      <li>
        <label for="title">Volume Number</label>
        <input type="text" id="volume" name="volume" value="${book.volume}">
      </li>
      <li>
        <label for="title">Fascicle Number</label>
        <input type="text" id="fascicle" name="fascicle" value="${book.fascicle}">
      </li>
      <li>
        <label for="title">Volume Title</label>
        <input type="text" id="voltitle" name="voltitle" value="${book.voltitle}">
      </li>
      <li>
        <label for="isbn" class="required">Author</label>
        <input type="text" id="author" name="author" value="${book.author}" required>
      </li>
      <li>
        <label for="read">Read</label>
% if book.read:
        <input type="checkbox" id="read" name="read" checked>
% else:
        <input type="checkbox" id="read" name="read">
% endif
      </li>
      <li>
        <label for="lastread">Last Read</label>
% if book.lastread == date.min:
        <input type="date" id="lastread" name="lastread">
% else:
        <input type="date" id="lastread" name="lastread" value="${book.lastread}">
% endif
      </li>
      <li>
        <label for="location" class="required">Location</label>
        <input type="text" id="location" name="location" value="${book.location}" required>
      </li>
      <li>
        <label for="borrower">Borrower</label>
        <input type="text" id="borrower" name="borrower" value="${book.borrower}">
      </li>
      <li>
        <label for="quote">Quote</label>
        <textarea id="quote" name="quote">${book.quote}</textarea>
      </li>
      <li>
        <label for="notes">Notes</label>
        <textarea id="notes" name="notes">${book.notes}</textarea>
      </li>
      <li>
        <input type="submit" value="${submit}">
      </li>
    </ol>
  </fieldset>
</form>

<div class="help">
  <p><span class="required">Required fields</span> must be filled in.</p>
  <p>Cover imges should be 225x150px, and be in png, gif, or jpg format.</p>
  <p>To enter multiple authors, separate them with an ampersand.</p>
</div>
