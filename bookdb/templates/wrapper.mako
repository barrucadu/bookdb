# -*- coding: utf-8 -*-
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
