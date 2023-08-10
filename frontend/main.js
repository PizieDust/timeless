fetchAndDisplayImages();
const pollingInterval = 3000;
let previousContent = [];
setInterval(fetchAndDisplayImages, pollingInterval);

async function fetchContent() {
  try {
    const response = await fetch("/content");
    const data = await response.json();
    console.log("Fetched content:", data);
    return data;
  } catch (error) {
    console.error("Error fetching content:", error);
    return [];
  }
}


async function fetchAndDisplayImages() {
  const content = await fetchContent();
  const imageGallery = document.getElementById("image-gallery");
  const emptyMessage = document.getElementById("no-img");

  if (content.length === 0) {
    emptyMessage.classList.remove("hidden")
    emptyMessage.classList.add("block");
  } else {
    emptyMessage.classList.remove("block")
    emptyMessage.classList.add("hidden")
    content.forEach(async item => {
      if (!previousContent.some(prevItem => prevItem.name === item.name)) {
        const imageDiv = document.createElement("div");
        imageDiv.classList.add("pb-1", "w-full", "bg-gray-200", "rounded-md");

        const image = document.createElement("img");
        image.classList.add("object-cover", "w-full", "h-96");
        image.src = `/image/${item.name}`;
        image.alt = item.caption;

        const caption = document.createElement("p");
        caption.classList.add("text-md", "font-semibold", "text-center");
        caption.textContent = item.caption;

        imageDiv.appendChild(image);
        imageDiv.appendChild(caption);
        imageGallery.appendChild(imageDiv);
      }
    });
  }

  previousContent = content;
}

document.addEventListener("DOMContentLoaded", () => {
  let imageInput = document.getElementById("imageInput");
  let button = document.getElementById("uploadButton");
  let caption = document.getElementById("imageCaption");
  imageInput.addEventListener("change", () => {
    let imageFiles = Array.from(imageInput.files);

    if (imageFiles.length === 0) {
      // No files selected
      return;
    }

    let formData = new FormData();
    formData.append("image", imageFiles[0]);
    formData.append("caption", caption.value); 

    // Disable the button temporarily to prevent multiple clicks
    button.disabled = true;

    // Send post request to your unikernel with the formData
    fetch("/upload", {
      method: "POST",
      body: formData,
      mode: "no-cors",
    })
    .then((response) => response.text())
      .then((responseText) => {
        const alertContainer = document.getElementById("alert-container");
        const alert = document.createElement("div");
        alert.className = "bg-green-500 text-white p-4 rounded-md transition ease-in-out delay-150 duration-300 my-2";
        alert.textContent = responseText;
        alertContainer.appendChild(alert);
        setTimeout(() => {
          alertContainer.removeChild(alert);
        }, 1600);
      })
      .catch((error) => {
        let alertContainer = document.getElementById("alert-container");
        let errorAlert = document.createElement("div");
        errorAlert.className = "alert bg-red-500 text-white transition ease-in-out delay-150 duration-300 my-2";
        errorAlert.textContent = "Error uploading image: " + error;
        alertContainer.appendChild(errorAlert);
        setTimeout(() => {
          alertContainer.removeChild(errorAlert);
        }, 1600);
      })
      .finally(() => {
        button.disabled = false;
      });
  });
})