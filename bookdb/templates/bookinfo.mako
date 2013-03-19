<%
import datetime
from markdown import markdown
from markupsafe import escape
%>

<%inherit file="wrapper.mako"/>

% if book.quote.strip() != "":
  <blockquote>
    ${book.quote}
  </blockquote>
% endif

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

% if book.notes.strip() != "":
  <div class="notes">
    ${markdown(escape(book.notes))|n}
  </div>
% endif

<ul class="meta">
  <li><a href="/${book.isbn}/edit">Edit</a></li>
  <li><a href="/${book.isbn}/delete">Delete</a></li>
</ul>
