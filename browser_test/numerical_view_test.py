import unittest
from pathlib import Path
from time import sleep

import pytest
from py._path.local import LocalPath
from selenium.webdriver import Firefox, FirefoxProfile
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.firefox.webdriver import WebDriver
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By

OPTS = Options()
OPTS.headless = True


@pytest.mark.usefixtures("download_dir")
class TestUI(unittest.TestCase):

    browser: WebDriver
    tmpdir_factory: pytest.TempdirFactory
    download_dir: LocalPath

    def setUp(self) -> None:

        profile = FirefoxProfile()
        profile.set_preference("browser.download.folderList", 2)
        profile.set_preference("browser.download.manager.showWhenStarting", False)
        profile.set_preference("browser.download.dir", str(self.download_dir))
        profile.set_preference(
            "browser.helperApps.neverAsk.saveToDisk", "image/png;text/csv"
        )

        self.browser = Firefox(options=OPTS, firefox_profile=profile)
        self.browser.get("http://localhost:3838/numerical/?variable=pglabgro")

    def test_second_dimension_not_displayed(self) -> None:
        second_dimension = self.browser.find_element(By.ID, "second_dimension")
        conditional_container = second_dimension.find_element(By.XPATH, "..")
        WebDriverWait(self.browser, 5).until(
            expected_conditions.invisibility_of_element(conditional_container)
        )
        self.assertEqual("none", conditional_container.value_of_css_property("display"))

    def test_second_dimension_displayed(self) -> None:

        second_dimension = self.browser.find_element(By.ID, "second_dimension")
        conditional_container = second_dimension.find_element(By.XPATH, "..")
        WebDriverWait(self.browser, 5).until(
            expected_conditions.presence_of_element_located(
                (By.XPATH, "//div[contains(@class, 'plot-container')]")
            )
        )
        first_dimension = WebDriverWait(self.browser, 10).until(
            expected_conditions.element_to_be_clickable(
                (By.XPATH, "//*[@id='first_dimension']/div/div/div/div[1]")
            )
        )

        first_dimension.click()
        dropdown_entry = WebDriverWait(self.browser, 10).until(
            expected_conditions.element_to_be_clickable(
                (By.XPATH, "//div[@data-value='alter_gr']")
            )
        )

        dropdown_entry.click()

        self.assertEqual("block", conditional_container.value_of_css_property("display"))

    def test_plot_download(self) -> None:

        plot_download_button = WebDriverWait(self.browser, 20).until(
            expected_conditions.visibility_of_element_located(
                (By.XPATH, "//a[@data-title='Download plot as a png']")
            )
        )
        plot_download_button.click()
        png_directory = Path(self.download_dir).joinpath("newplot.png")
        self.assertTrue(wait_for_file_to_exist(png_directory, 10))

        test_path = Path(__file__).parent.resolve()
        with open(test_path.joinpath("test_data/newplot.png"), "rb") as png:
            expected_png = png.read()
        with open(png_directory, "rb") as png:
            result_png = png.read()
        self.assertEqual(expected_png, result_png)

    def tearDown(self) -> None:
        self.browser.close()
        return super().tearDown()


def wait_for_file_to_exist(file_path: Path, timeout: int) -> bool:
    for _ in range(1, timeout + 1):
        if file_path.exists():
            return True
        sleep(1)
    return file_path.exists()
