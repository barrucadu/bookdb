<%
from utils.template import are_unread_books, next_book, prettyprint_book
%>
<!DOCTYLE html>
<html>
  <head>
    <title>${title}</title>

    <link rel="stylesheet" href="${webpath}/static/style.css" type="text/css">

    <script src="http://code.jquery.com/jquery-1.9.1.min.js"></script>
    <script src="http://code.jquery.com/ui/1.10.2/jquery-ui.js"></script>
    <script src="${webpath}/static/bookdb.js"></script>
  </head>

  <body>
    <div class="page">
      <header>
        <h1>${title}</h1>

        % if are_unread_books():
          <div id="next">
            <h2>Why not read…</h2>
            ${prettyprint_book(next_book())}
          </div>
        % endif

        <nav>
          <ul>
            <li><a href="${webpath}/">Home</a>
            <li><a href="${webpath}/add">Add</a>
            <li><a href="${webpath}/search">Search</a>
          </ul>
        </nav>
      </header>

      ${self.body()}

      <footer>
        <a href="https://github.com/barrucadu/bookdb">Github</a>
      </footer>
    </div>
  </body>
</html>
