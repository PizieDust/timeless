function uploadImage() {
    let imageInput = document.getElementById("imageInput");
    let button = document.getElementById("uploadButton");
    let caption = document.getElementById("imageCaption");
  
    // Trigger the file input's click event when the button is clicked
    button.addEventListener("click", () => {
      imageInput.click();
    });
  
    // Listen for the 'change' event on the file input
    imageInput.addEventListener("change", () => {
      let imageFiles = Array.from(imageInput.files);
  
      if (imageFiles.length === 0) {
        // No files selected
        return;
      }
  
      let formData = new FormData();
      formData.append("image", imageFiles[0]);
      formData.append("caption", caption.value); // Add the caption to the FormData
  
      // Send post request to your unikernel with the formData
      fetch("/upload", {
        method: "POST",
        body: formData,
        mode: "no-cors",
      })
        .then((response) => {
          // Handle response if needed
        })
        .catch((error) => {
          console.error("Error uploading image:", error);
        });
    });
  }
  