import {
  DesktopOutlined,
  FileOutlined,
  PieChartOutlined,
  TeamOutlined,
  UserOutlined,
} from '@ant-design/icons'
import { faker } from '@faker-js/faker'
import {
  Avatar,
  Badge,
  Breadcrumb,
  Button,
  Dropdown,
  Input,
  Layout,
  Menu,
  MenuProps,
  Typography,
} from 'antd'
import { capitalize } from 'lodash'
import { ReactNode, useContext, useState } from 'react'
import { Logo } from '../common/Logo'
import { FooterLove } from './common/FooterLove'

import colors from '../../colors.json'
import { AuthContext } from '../../auth'

interface PageTemplageProps extends HeaderProps, ContentProps {
  children: ReactNode
}

export function ProtectedTemplate({ children, title }: PageTemplageProps) {
  const { logout } = useContext(AuthContext)

  return (
    <Layout className="h-screen">
      <Layout.Header className="sticky top-0 p-0 h-20">
        <Header
          headerLeft={
            <Input.Search
              className="w-96"
              placeholder={faker.lorem.sentence(5)}
            />
          }
          headerRight={
            <Badge
              color={colors['primary-2']}
              count={12}
              offset={[-10, 0]}
              size="small"
            >
              <Dropdown
                menu={{
                  items: [
                    {
                      label: (
                        <Button
                          type="link"
                          onClick={logout}
                        >
                          Выйти
                        </Button>
                      ),
                      key: '0',
                    },
                  ],
                }}
                trigger={['click']}
              >
                <Avatar
                  size={48}
                  icon={<UserOutlined />}
                />
              </Dropdown>
            </Badge>
          }
        />
      </Layout.Header>

      <Layout>
        <Sider />

        <Layout>
          <Layout.Content className="bg-gray-100">
            <Content title={title}>{children}</Content>
          </Layout.Content>

          <Layout.Footer className="py-3.5">
            <FooterLove />
          </Layout.Footer>
        </Layout>
      </Layout>
    </Layout>
  )
}

interface HeaderProps {
  headerLeft?: ReactNode
  headerRight?: ReactNode
}

function Header({ headerLeft, headerRight }: HeaderProps) {
  return (
    <div className="flex items-center h-full border-b-4 border-primary-2">
      <div className="w-10 ml-5">
        <Logo />
      </div>
      <div className="flex h-full grow justify-between items-center px-10">
        {headerLeft}
        {headerRight}
      </div>
    </div>
  )
}

interface ContentProps {
  // breadcrumbs?: BreadcrumbProps['items']
  children: ReactNode
  title: ReactNode
}

function Content({ children, title }: ContentProps) {
  const breadcrumbs = [
    { title: 'Header left' },
    ...Array.from({ length: 2 }, () => ({ title: capitalize(faker.hacker.noun()) })),
  ]

  return (
    <div className="flex flex-col h-full">
      <Breadcrumb
        items={breadcrumbs}
        className="p-6"
      />

      <div className="overflow-auto scroll-smooth grow mx-6">
        <div className="mr-2 p-8 min-h-full bg-white rounded-md border border-slate-200">
          <Typography.Title>{title}</Typography.Title>
          {children}
        </div>
      </div>
    </div>
  )
}

function Sider() {
  const [siderCollapsed, setSiderCollapsed] = useState(true)

  type MenuItem = Required<MenuProps>['items'][number]

  function getItem(
    label: React.ReactNode,
    key: React.Key,
    icon?: React.ReactNode,
    children?: MenuItem[],
  ): MenuItem {
    return {
      key,
      icon,
      children,
      label,
    } as MenuItem
  }

  const items: MenuItem[] = [
    getItem(
      <Typography.Link>Option 1</Typography.Link>,
      '1',
      <PieChartOutlined className="!text-lg" />,
    ),
    getItem(
      <Typography.Link>Option 2</Typography.Link>,
      '2',
      <DesktopOutlined className="!text-lg" />,
    ),
    getItem(
      <Typography.Link>User</Typography.Link>,
      'sub1',
      <UserOutlined className="!text-lg" />,
      [
        getItem(<Typography.Link>Tom</Typography.Link>, '3'),
        getItem(<Typography.Link>Bill</Typography.Link>, '4'),
        getItem(<Typography.Link>Alex</Typography.Link>, '5'),
      ],
    ),
    getItem(
      <Typography.Link>Team</Typography.Link>,
      'sub2',
      <TeamOutlined className="!text-lg" />,
      [
        getItem(<Typography.Link>Team 1</Typography.Link>, '6'),
        getItem(<Typography.Link>Team 2</Typography.Link>, '8'),
      ],
    ),
    getItem(<Typography.Link>Files</Typography.Link>, '9', <FileOutlined className="!text-lg" />),
  ]
  return (
    <Layout.Sider
      className="!sticky w-24 border-r border-primary-2"
      collapsed={siderCollapsed}
      collapsible
      onCollapse={() => setSiderCollapsed(!siderCollapsed)}
      trigger={
        <Typography.Text className="font-black text-lg text-secondary-2">
          {siderCollapsed ? '>' : '<'}
        </Typography.Text>
      }
    >
      <Menu
        className="text-lg"
        defaultSelectedKeys={['1']}
        mode="inline"
        items={items}
      />
    </Layout.Sider>
  )
}
