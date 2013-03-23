<!DOCTYLE html>
<html>
  <head>
    <title>Confirm Delete</title>
    <link rel="stylesheet" href="${webpath}/static/style.css" type="text/css">
  </head>

  <body>
    <div class="information">
      <header>
        <h1>Confirm Delete</h1>
      </header>

      <p>Do you really want to delete <span class="isbn">${isbn}</span>
        (<strong>${book.title}</strong>, by <strong>${book.author}</strong>)?</p>

      <form action="${webpath}/${isbn}/delete" method="post">
        <input type="submit" value="Confirm Delete">
      </form>

      <footer>
        <a href="https://github.com/barrucadu/bookdb">Github</a>
      </footer>
    </div>
  </body>
</html>
