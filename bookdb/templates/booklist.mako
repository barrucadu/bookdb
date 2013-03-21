<%
import datetime
from markdown import markdown
from markupsafe import escape
from templates.utils import plural, find_book_image

percentread = round((read / len(books)) * 100) if len(books) > 0 else 0
%>

<%inherit file="wrapper.mako"/>

<p>Showing ${len(books)} ${plural(len(books), "book")} by ${authors}
  ${plural(authors, "author")}, of which ${read} (${percentread}%)
  ${plural(read, "has")} been read.</p>

<table>
  <thead>
    <th>ISBN</th>
    <th>Title</th>
    <th>Author</th>
    <th>Read</th>
    <th>Location</th>
    <th>Borrower</th>
    <th>Meta</th>
  </thead>
  <tbody>
    % if len(books) == 0:
      <tr>
        <td colspan="7">There are no books.</td>
      </tr>
    % else:
      % for book in books:
        <tr>
          <td class="isbn">${book.isbn}</td>
          <td>${book.title}</td>
          <td>
            <ol>
              % for author in book.authors():
                <li><a href="/author/${author}">${author}</a></li>
              % endfor
            </ol>
          <td>
            % if book.read:
              % if book.lastread == datetime.date.min:
                <a href="/read/yes">✔</a>
              % else:
                <a href="/read/yes">✔ (${book.lastread})</a>
              % endif
            % else:
              <a href="/read/no">✘</a>
            % endif
          </td>
          <td><a href="/location/${book.location}">${book.location}</a>
          <td><a href="/borrower/${book.borrower}">${book.borrower}</td>
          <td class="meta">
            <a href="#" class="toggle" id="${book.isbn}-toggle">[info]</a>
            <a href="/${book.isbn}/edit">[edit]</a>
            <a href="/${book.isbn}/delete">[delete]</a>
          </td>
        </tr>
        <tr id="${book.isbn}-toggle-tr" class="hidden">
          <td colspan="7">
            <div id="${book.isbn}-toggle-div" class="hidden">
              <img src="${find_book_image(book.isbn)}" alt="${book.title}, by ${book.author}">

              % if book.quote.strip() != "":
                <blockquote>
                  ${markdown(escape(book.quote))|n}
                </blockquote>
              % endif

              % if book.notes.strip() != "":
                <div class="notes">
                  ${markdown(escape(book.notes))|n}
                </div>
              % endif
            </div>
          </td>
        </tr>
      % endfor
    % endif
  </tbody>
</table>
