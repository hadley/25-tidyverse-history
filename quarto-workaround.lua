function Image(img)
    if img.attributes["needs-quarto-bug-workaround"] then
        img.attributes["height"] = "200"
        return img
    end
end