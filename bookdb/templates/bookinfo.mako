<%
import datetime
%>

<%inherit file="wrapper.mako"/>

<dl>
  <dt>ISBN</dt>
  <dd class="isbn">${book.isbn}</dd>

  <dt>Title</dt>
  <dd>${book.title}</dd>

  <dt>Author</dt>
  <dd>
    <ol>
      % for author in book.authors():
        <li><a href="/author/${author}">${author}</a></li>
      % endfor
    </ol>
  </dd>

  <dt>Read</dt>
  <dd>
    % if book.read:
      % if book.lastread == datetime.date.min:
        <a href="/read/yes">✔</a>
      % else:
        <a href="/read/yes">✔ (${book.lastread})</a>
      % endif
    % else:
      <a href="/read/no">✘</a>
    % endif
  </dd>

  <dt>Location</dt>
  <dd>${book.location}</dd>

  <dt>Borrower</dt>
  <dd>${book.borrower}</dd>
</dl>

<ul class="meta">
  <li><a href="/${book.isbn}/edit">Edit</a></li>
  <li><a href="/${book.isbn}/delete">Delete</a></li>
</ul>
