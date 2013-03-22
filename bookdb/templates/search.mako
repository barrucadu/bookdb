<%inherit file="booklist.mako"/>

<form method="get" action="${webpath}/search">
  <fieldset>
    <ol>
      <li>
        <label for="isbn">ISBN</label>
        <input type="text" id="isbn" name="isbn" value="${isbn}">
      </li>
      <li>
        <label for="title">Title</label>
        <input type="text" id="title" name="title" value="${booktitle}">
      </li>
      <li>
        <label for="author">Author</label>
        <input type="text" id="author" name="author" value="${author}">
      </li>
      <li>
        <label for="matchread">Match Read</label>
% if matchread == "yes":
        <input type="checkbox" id="matchread" name="matchread" value="yes" checked>
% else:
        <input type="checkbox" id="matchread" name="matchread" value="yes">
% endif
      </li>
      <li>
        <label for="matchunread">Match Unread</label>
% if matchunread == "yes":
        <input type="checkbox" id="matchunread" name="matchunread" value="yes" checked>
% else:
        <input type="checkbox" id="matchunread" name="matchunread" value="yes">
% endif
      </li>
      <li>
        <label for="location">Location</label>
        <input type="text" id="location" name="location" value="${location}">
      </li>
      <li>
        <label for="borrower">Borrower</label>
        <input type="text" id="borrower" name="borrower" value="${borrower}">
      </li>
      <li><input type="submit" value="Search"></li>
    </ol>
  </fieldset>
</form>

${parent.body()}
