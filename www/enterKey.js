$(document).keyup(function(event) {
  if ($("#recallInput").is(":focus") && (event.key == "Enter")) {
    $("#add").click();

  }
});

$(document).keyup(function(event) {
  if ($("#distractionInput").is(":focus") && (event.key == "Enter")) {
    $("#questionSubmit").click();

  }
});

Shiny.addCustomMessageHandler("recallFocus", recallFocus);

Shiny.addCustomMessageHandler("distractorFocus", distractorFocus);

function recallFocus(NULL){
  document.getElementById("recallInput").focus();
}

function distractorFocus(NULL) {
  document.getElementById("distractionInput").focus();
}