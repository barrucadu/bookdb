<%inherit file="wrapper.mako"/>

<div class="confirm">
  <p>Do you really want to delete <span class="isbn">${isbn}</span>
  (<strong>${title}, by ${author}</strong>)?</p>

  <form action="/${isbn}/delete" method="post">
    <input type="submit" value="Confirm Delete">
  </form>
</div>
