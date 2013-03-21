<%
from templates.utils import are_unread_books, next_book
%>
<!DOCTYLE html>
<html>
  <head>
    <title>${pagetitle}</title>

    <link rel="stylesheet" href="/static/style.css" type="text/css">

    <script src="http://code.jquery.com/jquery-1.9.1.min.js"></script>
    <script src="http://code.jquery.com/ui/1.10.2/jquery-ui.js"></script>
    <script src="/static/bookdb.js"></script>
  </head>

  <body>
    <div class="page">
      <header>
        <h1>${pagetitle}</h1>

        % if are_unread_books():
          <div id="next">
            <h2>Why not read…</h2>
            ${next_book().title}
          </div>
        % endif

        <nav>
          <ul>
            <li><a href="/">Home</a>
            <li><a href="/add">Add</a>
            <li><a href="/search">Search</a>
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
