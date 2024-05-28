import { Breadcrumb, ConfigProvider, Flex, Layout, Typography } from 'antd'
import { faker } from '@faker-js/faker'
import { capitalize } from 'lodash'
import './App.css'

const backgroundColor = '#305987'

const breadcrumbs = Array.from({ length: 4 }, () => ({ title: capitalize(faker.hacker.noun()) }))

function App() {
  return (
    <ConfigProvider
      theme={{
        components: {
          Layout: {
            triggerBg: backgroundColor,
            headerBg: 'transparent',
            siderBg: backgroundColor,
            bodyBg: '#fff',
            footerBg: '#fff',
          },
          Typography: {
            colorLink: '#69b1ff',
          },
        },
      }}
    >
      <Layout className="h-screen">
        <Layout.Sider
          className="text-white !sticky"
          collapsible
          width="25%"
        >
          <Flex className="pl-6 pt-6">Logo</Flex>
        </Layout.Sider>

        <Layout>
          <Layout.Header className="sticky top-0 p-6">
            <Flex
              align="center"
              className="h-full"
            >
              <Breadcrumb items={breadcrumbs} />
            </Flex>
          </Layout.Header>
          <Layout.Content className="p-6 overflow-y-auto scroll-smooth mr-1">
            <Typography.Title>{faker.lorem.sentence(5)}</Typography.Title>
            <Typography.Text>{faker.lorem.sentence(1500)}</Typography.Text>
          </Layout.Content>
          <Layout.Footer className="px-6 py-2">
            <Flex justify="center">
              <Typography.Text
                className="text-slate-500"
                code
              >
                Made with ❤️ by{' '}
                <a
                  className="!text-blue-400 !font-bold"
                  href="https://github.com/QuanticTeam/AtomSkills2024"
                >
                  QuanticTeam
                </a>
              </Typography.Text>
            </Flex>
          </Layout.Footer>
        </Layout>
      </Layout>
    </ConfigProvider>
  )
}

export default App
