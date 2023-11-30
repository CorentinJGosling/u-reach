document.addEventListener("DOMContentLoaded", function () {
  const sections = document.querySelectorAll("section");
  const navLinks = document.querySelectorAll("nav ul li");
  console.log(sections.length);
  console.log(navLinks.length);
  if (sections.length === navLinks.length) {
    function highlightNavLink() {
      sections.forEach((section, idx) => {
        const rect = section.getBoundingClientRect();
        const threshold = window.innerHeight * 0.5; // 30% of the screen height
        const isVisible = rect.top <= threshold && rect.bottom >= threshold;

        if (isVisible) {
          navLinks.forEach((link) => {
            link.classList.remove("active");
          });
          navLinks[idx].classList.add("active");
        }
      });
    }

    window.addEventListener("scroll", highlightNavLink);
    window.addEventListener("resize", highlightNavLink);
  } else {
    console.error(
      "Number of sections doesn't match number of navigation links."
    );
  }
});
var leadID = document.querySelector("#lead").value;
var teamID = document.querySelector("#team").value;
console.log(leadID);
console.log(teamID);

function CheckMail() {
  var boxID = document.getElementById("form_id");
  var leadID = document.getElementById("lead").value;
  var teamID = document.getElementById("team").value;
  const fail = document.querySelector(".formFail");
  const succ = document.querySelector(".formSent");

  if (leadID == "" || teamID == "") {
    fail.style.display = "block";
    // succ.style.display = "none";
  } else {
    SendMail();
    boxID.style.display = "none";
    succ.style.display = "block";
    fail.style.display = "none";
  }
}

function SendMail() {
  (function () {
    emailjs.init("Y8CeD4zQKXjt5nAMJ");
  })();
  var params = {
    leader: document.getElementById("lead").value,
    teamer: document.getElementById("team").value,
  };
  var serviceID = "service_wlfzelg";
  var templateID = "template_rj8nkui";
  emailjs.send(serviceID, templateID, params).then(function (res) {
    () => {
      alert(
        "Your Message has been sent to our team! If we did not reply within the next few days, do not hesitate to contact us at cgosling@parisnanterre.fr"
      );
      window.location.reload(false);
    },
      () => {
        alert(
          "Failed to send message. Please send you inquiry directly by email at cgosling@parisnanterre.fr"
        );
      };
  });
}
