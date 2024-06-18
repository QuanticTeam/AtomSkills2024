import {
  AlignLeftOutlined,
  ArrowUpOutlined,
  ClockCircleOutlined,
  TableOutlined,
  UserAddOutlined,
} from '@ant-design/icons'
import { BackTop, Badge, Tag, FloatButton, Layout, MenuProps, Space, Typography } from 'antd'
import { ReactNode, useEffect, useRef, useState } from 'react'
import { useTranslation } from 'react-i18next'

import { useLocation, useNavigate } from 'react-router-dom'
import { GuardAuthorized } from '~/shared/auth'
import { ROUTE_PATH_LESSONS, ROUTE_PATH_LOREM } from '~/shared/routing'
import { Breadcrumbs, Footer, HeaderAuthorized, Sidebar } from '~/widgets'

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

interface PageAuthorizedProps {
  actions?: { key: string; node: ReactNode }[]
  children: ReactNode
  title: ReactNode
  contentType?: 'default' | 'form'
}

export function PageAuthorized({
  actions = [],
  children,
  contentType = 'default',
  title,
}: PageAuthorizedProps) {
  const [sidebarCollapsed, setSidebarCollapsed] = useState(true)
  const navigate = useNavigate()
  const location = useLocation()

  const { t } = useTranslation(PageAuthorized.name)

  const [selectedSidebarMenuItems, setSelectedSidebarMenuItems] = useState<string[]>([])

  useEffect(() => {
    if (location.pathname.startsWith('/something')) setSelectedSidebarMenuItems(['/something'])
    if (location.pathname.startsWith('/lorem')) setSelectedSidebarMenuItems(['/lorem'])
  }, [location.pathname])

  const sidebarMenuItems: MenuItem[] = [
    getItem(
      <Typography.Link onClick={() => navigate(ROUTE_PATH_LESSONS)}>
        {t('sideLinkLessons')}
      </Typography.Link>,
      ROUTE_PATH_LESSONS,
      <TableOutlined className="!text-lg" />,
    ),
    getItem(
      <Typography.Link onClick={() => navigate(ROUTE_PATH_LOREM)}>
        {t('sideLinkLorem')}
      </Typography.Link>,
      ROUTE_PATH_LOREM,
      <AlignLeftOutlined className="!text-lg" />,
    ),
  ]

  const contentRef = useRef<HTMLDivElement>(null)

  return (
    <GuardAuthorized>
      <Layout className="h-screen">
        <HeaderAuthorized />

        {/* Keep flex row in advance to prevent ant computed layout flickering */}
        <Layout className="flex-row">
          <Sidebar
            collapsed={sidebarCollapsed}
            menuItems={sidebarMenuItems}
            onCollapse={() => setSidebarCollapsed(!sidebarCollapsed)}
            selectedMenuItems={selectedSidebarMenuItems}
          />

          <Layout>
            <Layout.Content className="bg-gray-100">
              <div className="flex flex-col h-full">
                <Breadcrumbs />

                <FloatButton.BackTop
                  target={() => contentRef.current!}
                  icon={<ArrowUpOutlined />}
                  style={{
                    bottom: '70px',
                    right: '60px',
                  }}
                />

                <div
                  className="overflow-auto scroll-smooth grow mx-6"
                  ref={contentRef}
                >
                  <div className="flex flex-col mr-2 p-8 min-h-full bg-white rounded-md border border-slate-200">
                    <div className="flex items-center justify-between">
                      <div className="grow">
                        {typeof title === 'string' ? (
                          <Typography.Title className="!mb-6">{title}</Typography.Title>
                        ) : (
                          title
                        )}
                      </div>
                      {!actions.length ? null : (
                        <Space
                          direction="horizontal"
                          className="TODO"
                        >
                          {actions.map(({ key, node }) => (
                            <div key={key}>{node}</div>
                          ))}
                        </Space>
                      )}
                    </div>
                    <div className="grow">{children}</div>
                  </div>
                </div>
              </div>
            </Layout.Content>

            <Layout.Footer className="py-3.5">
              <Footer />
            </Layout.Footer>
          </Layout>
        </Layout>
      </Layout>
    </GuardAuthorized>
  )
}
