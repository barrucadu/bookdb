<%
import datetime

percentread = round((read / len(books)) * 100) if len(books) > 0 else 0
%>

<%inherit file="wrapper.mako"/>

<p>Showing ${len(books)} books by ${authors} authors, of which ${read}
  (${percentread}%) have been read.</p>

<table>
  <thead>
    <th>ISBN</th>
    <th>Title</th>
    <th>Author</th>
    <th>Read</th>
    <th>Location</th>
    <th>Borrower</th>
  </thead>
  <tbody>
    % if len(books) == 0:
      <tr>
        <td colspan="6">There are no books.</td>
      </tr>
    % else:
      % for book in books:
        <tr>
          <td class="isbn">${book.isbn}</td>
          <td>${book.title}</td>
          <td>
            <ol>
              % for author in book.authors:
                <li><a href="/author/${author}">${author}</a></li>
              % endfor
            </ol>
          <td>
            % if book.read:
              % if book.lastread != datetime.date.min:
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
        </tr>
      % endfor
    % endif
  </tbody>
</table>
