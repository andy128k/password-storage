module.exports = function(eleventyConfig) {
  eleventyConfig.addPassthroughCopy("css");
  eleventyConfig.addPassthroughCopy("screenshots");
  eleventyConfig.addPassthroughCopy("favicon.png");
  eleventyConfig.addPassthroughCopy("icon.png");
  eleventyConfig.addPassthroughCopy("CNAME");

  return {
    pathPrefix: '/',
  };
};
