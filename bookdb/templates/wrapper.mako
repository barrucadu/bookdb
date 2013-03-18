# -*- coding: utf-8 -*-
<!DOCTYLE html>
<html>
  <head>
    <title>${title}</title>

    <link rel="stylesheet" href="/static/style.css" type="text/css">
  </head>

  <body>
    <div class="page">
      <header>
        <h1>${title}</h1>

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
